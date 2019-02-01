# What's the branch about

* The branch `stack-rtl-mjit` is used for development of **RTL
  (register transfer language)** VM insns and MRI JIT (**MJIT** in
  brief) of the RTL insns

  * Takashi Kokubun already implemented **stack insns** working with
    MJIT and merged this code into the trunk.  This branch is focused
    on MJIT working with RTL and implementation of new MJIT optimizations

* The last branch merge point with the trunk is always the head of the
  branch `stack-rtl-mjit-base`
    * The branch `stack-rtl-mjit` will be merged with the trunk from
      time to time and correspondingly the head of the branch
      `stack-rtl-mjit-base` will be the last merge point with the trunk


# RTL insns

* The major goal of RTL insns introduction is an implementation of **IR
  for Ruby code analysis and optimizations**
    * The current **stack based insns** are an inconvenient IR for such goal

* Secondary goal is faster interpretation of VM insns

    * Stack based insns create additional memory traffic.  Let us
      consider Ruby code `a = b + c`.  Stack insns vs RTL insns for
      the code:

```
                  getlocal_OP__WC__0 <b index>
                  getlocal_OP__WC__0 <c index>
                  opt_plus
                  setlocal_OP__WC__0 <a index>
```
vs   
```
                  plus <a index>, <b index>, <c index>
```


* Stack based insns are **shorter** but usually **require more
  insns** than RTL ones for the same Ruby code
    * We save time on memory traffic and insn dispatching when we use RTL
    * In some cases, RTL insns can be the same number as stack-based
      insns as typical Ruby code contains a lot of calls.  In such
      cases, executing RTL insns will be slower than executing stack insns

* Currently **we use only temporaries and locals** as RTL insn
  operands although more complicated operands (e.g. instance and class
  variables)

      
## RTL insn combining and specialization

* Immediate value specialization (e.g. `plusi` - addition with
  immediate fixnum)
      
* Frequent insn sequences combining (e.g. `bteq` - comparison and
  branch if the operands are equal)

  * As the comparison part of combined compare and branch insns might be an ISEQ call, we need
    to provide a solution to do an actual jump right after the call
    * If the comparison part is actually an ISEQ call, we change a return PC.  So
      the next insn executed after call will be a branch insn
    * To decrease memory overhead, this branch insn is a part of the
      original insn
        * For example, in the case of a call in "`bteq <cont_btcmp code>, <label>
          <call data>, <cmp result>, op1, op2`" the next executed insn will be
          "`cont_btcmp <label>, <call_data>, <cmp result>, op1, op2`"
 
## Speculative insn generation

* Some initially generated insns **during their execution** can be transformed
  into **speculative** ones
    * Speculation is based on **operand types** (e.g. plus can be
      transformed into an integer plus) and on the **operand values**
      (e.g. no multi-precision integers)

* Speculative insns can be transformed into **unchanging regular
  insns** if the speculation is wrong
    * Speculation insns have a code checking the speculation correctness

* Speculation is more important for JITed code performance

## Simple (stack) insns

* I've added new specialized RTL insns which can decrease RTL code
  size for numerous cases when operands are processed in a stack
  order:

```
                  plus <index -1>, <index -1>, <index -2>
```
vs   
```
                  splus <index -1>
```



# How we generate RTL insns

* The previous approach was to generate directly from CRuby parse
  tree nodes

* The current approach is to generate RTL insns from the stack insns
  because the stack insns are already a part of CRuby interface

* To generate an optimized RTL code, we solve a forward data flow
  problems to find what each stack slot contains at each program point

* File `rtl_gen.c` contains the major code for RTL insn generation

# A new JIT optimization

* **To demonstrate RTL advantage for Ruby code analysis and
  transformation**, I implemented removing code for boxing and
  unboxing floating point values

* By default the optimization is switched off.  To switch it on, use
  option `--jit-fpopt`

* For example, the optimization improves the following JITted code in 3.2 times:

```
    def f
      r = 2.0; m = 1.001
      i = 0
      while i < 1_000
        r *= m; r *= m; r *= m; r *= m; r *= m
        r *= m; r *= m; r *= m; r *= m; r *= m
        i += 1
      end
      r
    end

    r = 0.0
    100_000.times { r = f}
    p r
```


# RTL vs trunk performance as Jan. 25, 2019

* All measurements are done on Intel i7-9700K with 16GB memory under
  x86-64 Fedora Core29

* I compared the following CRuby versions:
    * trunk - `stack-rtl-mjit-base` branch which is trunk as of Dec. 17, 2018
    * RTL branch - `stack-rtl-mjit` branch containing RTL work and all trunk changes
      on branch `stack-rtl-mjit-base`

* I used the following micro-benchmarks (see MJIT-benchmarks directory):
    * while - while loop
    * nest-while - nested while loops (6 levels)
    * nest-ntimes - nested ntimes loops (6 levels)
    * ivread - reading an instance variable (@var)
    * ivwrite - assignment to an instance variable
    * aread - reading an instance variable through attr_reader
    * awrite - assignment to an instance variable through attr_writer
    * aref - reading an array element
    * aset - assignment to an array element
    * const - reading Const
    * const2 - reading Class::Const 
    * call - empty method calls
    * fib - fibonacci
    * fannk - fannkuch
    * sieve - Eratosthenes sieve
    * mandelbrot - (non-complex) mandelbrot as CRuby v2 does not support complex numbers
    * meteor - meteor puzzle
    * nbody - modeling planet orbits
    * norm - spectral norm
    * trees - binary trees
    * pent - pentamino puzzle
    * red-black - Red Black trees
    * bench - rendering

* Each benchmark ran 3 times and minimal time (or smallest maximum resident memory) was chosen

* I also used optcarrot for more serious program performance
  comparison
    * I used **2000** frames to run optcarrot

## Microbenchmark results in interpreter mode

* Wall time speedup ('wall trunk time' / 'wall RTL time'):

---

|                     | trunk  | RTL branch |
:---------------------|-------:|-----------:|
aread.rb              | 1.0    | 1.74       |
aref.rb               | 1.0    | 2.06       |
aset.rb               | 1.0    | 2.02       |
awrite.rb             | 1.0    | 1.61       |
bench.rb              | 1.0    | 1.16       |
call.rb               | 1.0    | 1.35       |
complex-mandelbrot.rb | 1.0    | 1.13       |
const2.rb             | 1.0    | 1.87       |
const.rb              | 1.0    | 2.04       |
fannk.rb              | 1.0    | 1.08       |
fib.rb                | 1.0    | 1.34       |
ivread.rb             | 1.0    | 1.42       |
ivwrite.rb            | 1.0    | 1.81       |
mandelbrot.rb         | 1.0    | 1.28       |
meteor.rb             | 1.0    | 1.02       |
nbody.rb              | 1.0    | 1.26       |
nest-ntimes.rb        | 1.0    | 1.02       |
nest-while.rb         | 1.0    | 1.96       |
norm.rb               | 1.0    | 1.1        |
nsvb.rb               | 1.0    | 1.0        |
pent.rb               | 1.0    | 1.1        |
sieve.rb              | 1.0    | 1.5        |
trees.rb              | 1.0    | 1.17       |
while.rb              | 1.0    | 2.15       |
GeoMean.              | 1.0    | 1.42       |

---

* CPU time improvements ('CPU trunk time' / 'CPU RTL time'):
  
---

|                     | trunk  | RTL branch |
:---------------------|-------:|-----------:|
aread.rb              | 1.0    | 1.75       |
aref.rb               | 1.0    | 2.06       |
aset.rb               | 1.0    | 2.02       |
awrite.rb             | 1.0    | 1.61       |
bench.rb              | 1.0    | 1.16       |
call.rb               | 1.0    | 1.35       |
complex-mandelbrot.rb | 1.0    | 1.13       |
const2.rb             | 1.0    | 1.87       |
const.rb              | 1.0    | 2.04       |
fannk.rb              | 1.0    | 1.08       |
fib.rb                | 1.0    | 1.34       |
ivread.rb             | 1.0    | 1.42       |
ivwrite.rb            | 1.0    | 1.81       |
mandelbrot.rb         | 1.0    | 1.28       |
meteor.rb             | 1.0    | 1.01       |
nbody.rb              | 1.0    | 1.26       |
nest-ntimes.rb        | 1.0    | 1.02       |
nest-while.rb         | 1.0    | 1.96       |
norm.rb               | 1.0    | 1.1        |
nsvb.rb               | 1.0    | 1.0        |
pent.rb               | 1.0    | 1.1        |
sieve.rb              | 1.0    | 1.51       |
trees.rb              | 1.0    | 1.17       |
while.rb              | 1.0    | 2.15       |
GeoMean.              | 1.0    | 1.42       |

---

* Peak memory increase ('max resident RTL memory' / 'max resident trunk memory'):

---

|                     | trunk  | RTL branch |
:---------------------|-------:|-----------:|
aread.rb              | 1.0    | 1.03       |
aref.rb               | 1.0    | 1.05       |
aset.rb               | 1.0    | 1.05       |
awrite.rb             | 1.0    | 1.03       |
bench.rb              | 1.0    | 1.05       |
call.rb               | 1.0    | 1.02       |
complex-mandelbrot.rb | 1.0    | 1.04       |
const2.rb             | 1.0    | 1.03       |
const.rb              | 1.0    | 1.04       |
fannk.rb              | 1.0    | 1.04       |
fib.rb                | 1.0    | 1.03       |
ivread.rb             | 1.0    | 1.04       |
ivwrite.rb            | 1.0    | 1.04       |
mandelbrot.rb         | 1.0    | 1.04       |
meteor.rb             | 1.0    | 1.06       |
nbody.rb              | 1.0    | 1.04       |
nest-ntimes.rb        | 1.0    | 1.01       |
nest-while.rb         | 1.0    | 1.03       |
norm.rb               | 1.0    | 1.05       |
nsvb.rb               | 1.0    | 1.04       |
pent.rb               | 1.0    | 1.02       |
sieve.rb              | 1.0    | 1.0        |
trees.rb              | 1.0    | 0.97       |
while.rb              | 1.0    | 1.06       |
GeoMean.              | 1.0    | 1.03       |

---

## Microbenchmark results in JIT mode

* Wall time speedup ('wall trunk time' / 'wall RTL time'):

---

|                     | trunk  | RTL branch |
:---------------------|-------:|-----------:|
aread.rb              | 1.0    | 2.37       |
aref.rb               | 1.0    | 2.07       |
aset.rb               | 1.0    | 1.95       |
awrite.rb             | 1.0    | 2.38       |
bench.rb              | 1.0    | 1.49       |
call.rb               | 1.0    | 1.05       |
complex-mandelbrot.rb | 1.0    | 1.22       |
const2.rb             | 1.0    | 1.98       |
const.rb              | 1.0    | 2.21       |
fannk.rb              | 1.0    | 1.08       |
fib.rb                | 1.0    | 0.97       |
ivread.rb             | 1.0    | 1.81       |
ivwrite.rb            | 1.0    | 1.41       |
mandelbrot.rb         | 1.0    | 1.57       |
meteor.rb             | 1.0    | 1.3        |
nbody.rb              | 1.0    | 1.71       |
nest-ntimes.rb        | 1.0    | 1.4        |
nest-while.rb         | 1.0    | 1.47       |
norm.rb               | 1.0    | 1.29       |
nsvb.rb               | 1.0    | 1.08       |
pent.rb               | 1.0    | 1.2        |
sieve.rb              | 1.0    | 1.2        |
trees.rb              | 1.0    | 1.37       |
while.rb              | 1.0    | 2.83       |
GeoMean.              | 1.0    | 1.53       |

---

* CPU time improvements ('CPU trunk time' / 'CPU RTL time'):
  
---

|                     | trunk  | RTL branch |
:---------------------|-------:|-----------:|
aread.rb              | 1.0    | 2.41       |
aref.rb               | 1.0    | 2.11       |
aset.rb               | 1.0    | 1.7        |
awrite.rb             | 1.0    | 2.43       |
bench.rb              | 1.0    | 1.22       |
call.rb               | 1.0    | 1.05       |
complex-mandelbrot.rb | 1.0    | 1.09       |
const2.rb             | 1.0    | 2.13       |
const.rb              | 1.0    | 2.28       |
fannk.rb              | 1.0    | 0.97       |
fib.rb                | 1.0    | 0.97       |
ivread.rb             | 1.0    | 1.83       |
ivwrite.rb            | 1.0    | 1.43       |
mandelbrot.rb         | 1.0    | 1.32       |
meteor.rb             | 1.0    | 1.25       |
nbody.rb              | 1.0    | 1.71       |
nest-ntimes.rb        | 1.0    | 1.24       |
nest-while.rb         | 1.0    | 1.44       |
norm.rb               | 1.0    | 1.28       |
nsvb.rb               | 1.0    | 0.97       |
pent.rb               | 1.0    | 1.21       |
sieve.rb              | 1.0    | 1.18       |
trees.rb              | 1.0    | 1.38       |
while.rb              | 1.0    | 2.9        |
GeoMean.              | 1.0    | 1.48       |

---

* Peak memory increase ('max resident RTL memory' / 'max resident trunk memory'):

---

|                     | trunk  | RTL branch |
:---------------------|-------:|-----------:|
aread.rb              | 1.0    | 1.17       |
aref.rb               | 1.0    | 1.18       |
aset.rb               | 1.0    | 1.18       |
awrite.rb             | 1.0    | 1.18       |
bench.rb              | 1.0    | 1.1        |
call.rb               | 1.0    | 1.16       |
complex-mandelbrot.rb | 1.0    | 1.18       |
const2.rb             | 1.0    | 1.18       |
const.rb              | 1.0    | 1.16       |
fannk.rb              | 1.0    | 1.15       |
fib.rb                | 1.0    | 1.18       |
ivread.rb             | 1.0    | 1.12       |
ivwrite.rb            | 1.0    | 1.15       |
mandelbrot.rb         | 1.0    | 1.17       |
meteor.rb             | 1.0    | 1.18       |
nbody.rb              | 1.0    | 1.08       |
nest-ntimes.rb        | 1.0    | 1.16       |
nest-while.rb         | 1.0    | 1.17       |
norm.rb               | 1.0    | 1.18       |
nsvb.rb               | 1.0    | 1.15       |
pent.rb               | 1.0    | 1.19       |
sieve.rb              | 1.0    | 0.84       |
trees.rb              | 1.0    | 1.22       |
while.rb              | 1.0    | 1.08       |
GeoMean.              | 1.0    | 1.14       |

---

## Optcarrot results

* I used 2000 frames and time instead of FPS:

* Wall time speedup ('wall trunk time' / 'wall RTL time'):

---

|                                  | trunk  | RTL branch |
:----------------------------------|-------:|-----------:|
optcarrot                          | 1.0    | 1.01       |
optcarrot --opt                    | 1.0    | 1.24       |
optcarrot (ruby --jit)             | 1.0    | 1.14       |
optcarrot --opt (ruby --jit)       | 1.0    | 1.30       |

---

* CPU time ('CPU trunk time' / 'CPU RTL time'):

---

|                                  | trunk  | RTL branch |
:----------------------------------|-------:|-----------:|
optcarrot                          | 1.0    | 1.01       |
optcarrot --opt                    | 1.0    | 1.25       |
optcarrot (ruby --jit)             | 1.0    | 1.14       |
optcarrot --opt (ruby --jit)       | 1.0    | 1.32       |

---

* Peak Memory ('max resident RTL memory' / 'max resident trunk memory'):

---

|                                  | trunk  | RTL branch |
:----------------------------------|-------:|-----------:|
optcarrot                          | 1.0    | 1.06       |
optcarrot --opt                    | 1.0    | 1.05       |
optcarrot (ruby --jit)             | 1.0    | 1.09       |
optcarrot --opt (ruby --jit)       | 1.0    | 0.99       |

