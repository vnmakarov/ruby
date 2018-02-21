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

## RTL insn operands

* What could be an operand:
    * only temporaries
    * temporaries and locals
    * temporaries and locals even from higher levels
    * above + instance variables
    * above + class variables, globals

* Using only temporaries does not make sense as it will produce code
  with practically the same number insns which are only longer

* Decoding overhead of numerous type operands will be not
  compensated by processing smaller number of insns

* The complicated operands also complicate optimizations and MJIT

* Currently **we use only temporaries and locals** as preliminary
  experiments show that it is the best approach
      
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

* Speculation will be more important for JITed code performance
  
## Two approaches to generate RTL insns:

* The one way is to generate RTL insns from the stack insns
* An another and a **faster** approach is to generate directly from CRuby parse
  tree nodes
* We use the first approach as the stack insns are already a part of CRuby interface

## RTL insns status and future work

* It mostly works (`make check` reports no regressions)

* Still a lot of work should be done for performance analysis and
  performance tuning work

* There are a lot of changed files but major changes are in:

    * `insns.def`: New definitions of RTL insns
    * `rtl_exec.c`: Most of code executing RTL insns
    * `rtl_gen.c`: Translations of the stack insns into RTL insns

---

# CRuby JIT

## A few possible approaches in JIT implementation:

* JIT specialized for a specific language (e.g. luajit, rujit)
    * Pro: achievability of very fast compilation
    * Con: a lot of efforts to implement decent optimizations and
      multi-target generation
          
* Using existing VMs with JIT or JIT libraries: Oracle JVM and Graal, IBM OMR,
  different JavaScript JITs, libjit
    * Pro: saving a lot of efforts
    * Cons: Big dependency on code which is hard to control.
      Less optimized code than a code generated by C compilers
      (even in comparsion with JVM server compiler).
      Most of the JITs are already used for Ruby implementation

* Using JITs frameworks of existing C compilers: GCC JIT, LLVM JIT
  engines
    * Pro: saving a lot of efforts in generating highly optimized
      code for multiple targets.  No new dependencies as C
      compilers are used for building CRuby
    * Cons: Unstable interfaces. An LLVM JIT is already used by
      Rubicon.  A lot of efforts in preparation of code used by
      RTL insns (**an environment**)
          
* Using existing C compilers
    * Pro: Very stable interface.  It is the simplest approach to
      generate highly optimized code for multiple targets (minimal
      changes to CRuby).  Small efforts to prepare the environment.
      Portability (e.g. GCC or LLVM can be used).  No new dependencies.
      Easy JITed code debugging.  Rich optimization set of
        industrial C compilers has a potential to generate a
        better code especially if we manage to provide profile
        info to them
    * Con: Big JIT code compilation time because of time spent on
      lexical, syntax, semantic analysis and optimizations not
      tailored for the speedy work

* The above is just a very brief analysis resulting in me to use the
  last approach.  It is the simplest one and adequate for long running
  Ruby programs like Ruby on Rails
    * Spending efforts to speed up the compilation
  
## MJIT organization


```
  _______     _________________
 |header |-->| minimized header|
 |_______|   |_________________|
               |                         CRuby building
 --------------|----------------------------------------
               |                         CRuby execution
               |                                            
  _____________|_____
 |             |     |
 |          ___V__   |  CC      ____________________
 |         |      |----------->| precompiled header |
 |         |      |  |         |____________________|
 |         |      |  |              |
 |         | MJIT |  |              |
 |         |      |  |              |
 |         |      |  |          ____V___  CC  __________
 |         |______|----------->| C code |--->| .so file |
 |                   |         |________|    |__________|
 |                   |                              |
 |                   |                              |
 | CRuby machine code|<-----------------------------
 |___________________|             loading

```

* MJIT is a **method JIT** (one more reason for the name)

* An important organization goal is to minimize the JIT compilation time

* To simplify JIT implementation the environment (C code header needed
  to C code generated by MJIT) is just `vm.c` file for now

* A special Ruby **script minimize the environment**
    * Removing about 90% declarations

* MJIT has a several threads (workers) to do **parallel compilations**
    * One worker prepares a **precompiled code of the minimized header**
        * It starts at the CRuby execution start

    * One or more workers generate PIC object files of ISEQs
        * They start when the precompiled header is ready
        * They take ISEQs from a **priority queue** unless it is empty.
        * They translate ISEQs into C-code using the precompiled header,
          call CC and load PIC code when it is ready

* MJIT puts ISEQ in the queue when ISEQ is called or right after
  generating ISEQ for AOT (**Ahead Of Time** compilation) 

* MJIT can **reorder ISEQs in the queue** if some ISEQ has been called many
  times and its compilation did not start yet or we need the ISEQ code
  for AOT

* CRuby reuses the machine code if it already exists for ISEQ

* All files are stored in `/tmp`.  On modern Linux `/tmp` is a file
  system in memory

* The machine code execution **can stop and switch to the ISEQ
  interpretation** if some condition is not satisfied as the machine
  code can be speculative or some exception raises

* Speculative machine code can be **canceled**, and a new **mutated**
  machine code can be queued for creation
    * It can happen when insn speculation was wrong
    * There is a constraint on the mutation number.  The default
      value can be changed by a MJIT option.  The last mutation will
      contain the code without any speculation insns

* There are more speculations in JIT code than in the interpreter mode:
    * Global speculation about tracing
    * Global speculation about absence of basic type operations redefinition
    * Speculation about equality of EP (environment pointer)
      and BP (basic stack pointer)

* When a global speculation becomes wrong, all currently executed JIT
  functions are canceled and the corresponding ISEQs continue their
  execution in the interpreter mode
    * It is implemented by checking a special control frame flag after
      each call which can affect a global speculation

* In AOT mode, ISEQ JIT code creation is queued
  right after the ISEQ creation and ISEQ JIT code is always tried to be
  executed first.  In other words, VM waits the creation of JIT code
  if it is not available
    * Now AOT probably has a sense mostly for MJIT debugging and may
      be for big long running programs

* MJIT options can be given on the command line or by environment
  variable RUBYOPT (the later probably will be removed in the future)

## RTL MJIT status

* **It is still on early stages of the development and only ready for
  usage for a few small and simple Ruby programs**
    * `make test` has no issues
    * The compilation of small ISEQ takes about 50-70 ms on modern
      x86-64 CPUs
    * No Ruby program real time execution slow down because of MJIT
    * Depending on a MJIT option, GCC or LLVM is used
        * Some benchmarks are faster with GCC, some are faster with
          LLVM Clang
        * There are a few factors (mostly relation between compilation
          speed and generated code quality) making hard to predict the
          outcome
        * As GCC and LLVM are ABI compatible you can compile CRuby by GCC
          and use LLVM for MJIT or vise verse

    * MJIT is switched on by `-j` option
    * Some other useful MJIT options:
        * `-j:v` helps to see how MJIT works: what ISEQs and when are
          compiled
        * `-j:p` prints a final profile about how frequently ISEQs were
          executed in the interpreter and JIT mode
        * `-j:a` switches on MJIT AOT mode
        * `-j:s` saves the precompiled header and all C files and object
          files in `/tmp` after CRuby finish
        * `-j:t=N` defines number of threads used by MJIT to compile
          ISEQs in parallel (default N is 1)
        * Use ruby option `--help` to see all MJIT options

## RTL MJIT future works

* A lot of things should be done to use RTL MJIT.  Here are the high
  priority ones:

    * Make it working for `make check`

    * Generation of optimized C code:
        * The ultimate goal is to provide possibility of **inlining on
          paths `Ruby->C->Ruby`** where Ruby means C code generated by MJIT
          for user defined Ruby methods and C means CRuby C code implementing
          some predefined Ruby methods (e.g. `times` for `Number`)
        * More aggressively speculative C code generation with more
          possibilities for C compiler optimizations, e.g. speculative
          constant usage for C compiler constant folding, (conditional) constant
          propagation, etc.
        * Translations of Ruby temporaries and locals into C locals and
          saving them on CRuby thread stack in case of deoptimization
            * Direct calls of C functions generated for ISEQs by MJIT
              (another form of speculations)
        * Transition from `static inline` functions to `extern inline`
          for GCC and Clang to permit the compilers themselves decide
          about inlining profitability
        * Pass profile info through hot/cold function attributes
             * May be pass more detail info through C compiler profile
               info format in the future
        
    * Tuning MJIT for faster compilation and less waiting time
    
    * Implementing **On Stack Replacement** (OSR)
        * OSR is a replacement of still executed byte code ISEQ by JIT
          generated machine code for the ISEQ
        * It is a low priority task as it is usable now only for ISEQs
          with while-statements

    * Tailor MJIT for a server environment
        * Reuse the same ISEQ JIT code for different running CRuby instances
        * Use a crypto-hash function to search JIT code for given pair
         (PCH hash, ISEQ hash)

    * MJIT vulnerability
        * Prevent adversary from changing C compiler
        * Prevent adversary from changing MJIT C and object files
        * Prevent adversary from changing MJIT headers
            * Use crypto hash function to check the header authenticity

# RTL MJIT performance as Feb. 20, 2018

* RTL MJIT is reliable enough to run some benchmarks to evaluate its
  potential

* All measurements are done on Intel 3.9GHz i3-7100 with 32GB memory
  under x86-64 Fedora Core25

* For the performance comparison I used the following implementations:
    * v2 - CRuby version 2.0
    * base - CRuby (2.5 development) version on which `stack-rtl-mjit` branch
      is based
    * rtl - `stack-rtl-mjit` branch as of Feb. 18 **without** using JIT
    * mjit - as above but **with** using MJIT with GCC 6.3.1 with `-O2`
    * mjit-cl - MJIT using LLVM Clang 3.9.1 with `-O2`
    * omr - Ruby OMR (2016-12-24 revision 57163) in JIT mode (`-Xjit`)
    * jruby9k - JRruby version 9.1.8.0
    * jruby9k-d - as above but with using `-Xcompile.invokedynamic=true`
    * graal-31 - Graal Ruby version 0.31
    * graal-31-n - Graal Ruby version 0.31 wit using `--native`
        * `--native` significantly decrease warm-up time

* I used the following micro-benchmarks (se MJIT-benchmarks directory):
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

* MJIT has a very fast startup which is not true for JRuby and Graal Ruby
    * To give a better chance to JRuby and Graal Ruby the benchmarks
      were modified in a way that Ruby CRuby v2.0 runs about 20s-70s on
      each benchmark

* Each benchmark ran 3 times and the minimal time (or minimal peak memory
  consumption) was chosen
    * In the tables for times I use ratio `<CRuby v2.0 time>/time`.  It
      show how the particular implementation is faster than CRuby v2.0
    * For memory I use ratio `<peak memory>/<CRuby v2.0 peak memory>`
      which shows how the particular implementation is memory hungrier
      than CRuby v2.0
    
* I also used optcarrot for more serious program performance
  comparison
    * I used **default** frames number (180) and **2000** frames to run
      optcarrot

## Microbenchmark results

* Wall time speedup ('wall CRuby v2.0 time' / 'wall time')
    * MJIT gives a real speedup comparable with JRuby
    * OMR is currently the worst performance JIT
    * In most cases, using GCC is better choice for MJIT than LLVM
    * Graal with `--native` gives incredibly fast code for some micro-benchmarks (ones from `while.rb` to `call.rb`):
        * The speedup is achieved by removing loops or moving
          repetitive calculations out of the loops.  For example, on
          `aset.rb` MJIT now achieves 70% performance of analogous typed **C**
          code compiled with `gcc -O0`. `gcc -O1` removes stores out
          of the loop and results in 10 times faster code.  `gcc -O2'
          additionally removes the empty loop and makes code practically instant
        * I suspect that future implementation of inlining in MJIT
          will result in analogous performance code.  *Implementation
          of inlining in MJIT should be the highest priority task* to
          be closer to Graal in wall time performance

---

|                   | v2         | base       | rtl        | mjit       | mjit-cl    | omr        | jruby9k    | jruby9k-d  | graal-31   | graal-31-n |
:-------------------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
while.rb            | 1.0        | 1.11       | 2.28       | 12.64      | 10.13      | 1.06       | 2.39       | 2.83       | 9.39       | 245.82     |
nest-while.rb       | 1.0        | 1.1        | 1.8        | 4.61       | 4.48       | 1.04       | 1.57       | 2.61       | 5.59       | 38.17      |
nest-ntimes.rb      | 1.0        | 0.98       | 1.14       | 2.11       | 2.3        | 0.99       | 0.97       | 0.97       | 5.56       | 28.45      |
ivread.rb           | 1.0        | 1.17       | 2.49       | 13.89      | 11.04      | 1.16       | 2.35       | 3.06       | 10.03      | 246.5      |
ivwrite.rb          | 1.0        | 1.25       | 2.23       | 7.83       | 7.71       | 1.15       | 2.57       | 3.05       | 6.73       | 128.63     |
aread.rb            | 1.0        | 1.02       | 1.63       | 8.94       | 7.29       | 0.96       | 1.78       | 3.48       | 6.98       | 142.4      |
awrite.rb           | 1.0        | 1.09       | 1.68       | 8.51       | 7.76       | 0.96       | 1.99       | 3.8        | 7.81       | 130.26     |
aref.rb             | 1.0        | 1.13       | 2.34       | 11.72      | 10.25      | 1.1        | 1.86       | 3.72       | 13.68      | 255.0      |
aset.rb             | 1.0        | 1.47       | 3.43       | 14.08      | 12.79      | 1.44       | 3.41       | 4.43       | 22.6       | 388.95     |
const.rb            | 1.0        | 1.1        | 2.06       | 11.6       | 10.58      | 1.08       | 3.02       | 3.95       | 22.02      | 321.95     |
const2.rb           | 1.0        | 1.12       | 2.09       | 11.79      | 10.06      | 1.1        | 3.08       | 3.85       | 22.03      | 327.33     |
call.rb             | 1.0        | 1.14       | 1.77       | 4.28       | 4.28       | 0.9        | 2.13       | 4.99       | 8.53       | 141.28     |
fib.rb              | 1.0        | 1.21       | 1.63       | 4.1        | 3.84       | 1.1        | 2.14       | 4.99       | 5.41       | 24.5       |
fannk.rb            | 1.0        | 1.05       | 1.12       | 1.11       | 1.13       | 1.03       | 1.73       | 2.36       | 2.5        | 2.92       |
sieve.rb            | 1.0        | 1.29       | 2.03       | 3.45       | 3.3        | 1.27       | 1.5        | 2.45       | 3.7        | 4.64       |
mandelbrot.rb       | 1.0        | 0.94       | 1.21       | 1.89       | 1.93       | 1.08       | 1.04       | 1.56       | 5.79       | 38.19      |
meteor.rb           | 1.0        | 1.22       | 1.33       | 1.62       | 1.62       | 1.15       | 0.88       | 0.94       | 0.84       | 0.76       |
nbody.rb            | 1.0        | 1.02       | 1.29       | 2.46       | 2.61       | 1.25       | 0.98       | 2.28       | 4.47       | 21.66      |
norm.rb             | 1.0        | 1.14       | 1.27       | 2.5        | 2.46       | 1.15       | 0.91       | 1.44       | 3.52       | 13.74      |
trees.rb            | 1.0        | 1.13       | 1.33       | 2.22       | 2.09       | 1.21       | 1.4        | 1.53       | 1.29       | 2.29       |
pent.rb             | 1.0        | 1.12       | 1.34       | 1.64       | 1.64       | 1.13       | 0.59       | 0.73       | 0.59       | 1.19       |
red-black.rb        | 1.0        | 0.99       | 0.99       | 1.31       | 1.22       | 0.89       | 0.99       | 2.43       | 1.93       | 4.7        |
bench.rb            | 1.0        | 1.15       | 1.22       | 1.64       | 1.81       | 1.14       | 1.29       | 2.8        | 3.68       | 8.84       |
GeoMean.            | 1.0        | 1.12       | 1.64       | 4.21       | 3.99       | 1.1        | 1.6        | 2.48       | 5.19       | 32.83      |

---

* CPU time improvements ('CPU CRuby v2.0 time' / 'CPU time')
    * CPU time is important too for cloud (money) or mobile (battery)
    * MJIT almost always spends less CPU than the current CRuby interpreter
    * Graal without `--native` is too aggressive with compilations and needs sometimes
      more CPU work than CRuby interpreter
  
---

|                   | v2         | base       | rtl        | mjit       | mjit-cl    | omr        | jruby9k    | jruby9k-d  | graal-31   | graal-31-n |
:-------------------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
while.rb            | 1.0        | 1.11       | 2.28       | 12.28      | 9.68       | 1.06       | 2.01       | 2.31       | 2.68       | 245.64     |
nest-while.rb       | 1.0        | 1.1        | 1.81       | 4.46       | 4.3        | 1.04       | 1.3        | 1.95       | 1.56       | 34.27      |
nest-ntimes.rb      | 1.0        | 0.98       | 1.14       | 2.05       | 2.22       | 0.99       | 0.88       | 0.87       | 1.54       | 19.38      |
ivread.rb           | 1.0        | 1.17       | 2.49       | 13.37      | 10.59      | 1.16       | 2.0        | 2.46       | 2.86       | 227.31     |
ivwrite.rb          | 1.0        | 1.25       | 2.23       | 7.61       | 7.4        | 1.15       | 1.97       | 2.23       | 1.92       | 120.94     |
aread.rb            | 1.0        | 1.02       | 1.63       | 8.67       | 7.0        | 0.96       | 1.48       | 2.56       | 1.99       | 133.38     |
awrite.rb           | 1.0        | 1.09       | 1.68       | 8.27       | 7.47       | 0.96       | 1.65       | 2.83       | 2.23       | 123.65     |
aref.rb             | 1.0        | 1.13       | 2.34       | 11.46      | 9.98       | 1.1        | 1.69       | 3.09       | 3.86       | 240.67     |
aset.rb             | 1.0        | 1.47       | 3.43       | 13.88      | 12.52      | 1.44       | 3.08       | 3.89       | 6.38       | 335.68     |
const.rb            | 1.0        | 1.1        | 2.06       | 11.45      | 10.41      | 1.08       | 2.77       | 3.48       | 6.22       | 293.74     |
const2.rb           | 1.0        | 1.12       | 2.09       | 11.66      | 9.9        | 1.1        | 2.8        | 3.42       | 6.27       | 312.23     |
call.rb             | 1.0        | 1.14       | 1.77       | 4.21       | 4.17       | 0.9        | 1.78       | 3.57       | 2.43       | 158.81     |
fib.rb              | 1.0        | 1.21       | 1.63       | 4.04       | 3.77       | 1.1        | 1.82       | 3.54       | 1.84       | 22.45      |
fannk.rb            | 1.0        | 1.05       | 1.12       | 1.11       | 1.12       | 1.03       | 1.46       | 1.78       | 1.04       | 2.63       |
sieve.rb            | 1.0        | 1.29       | 2.03       | 3.42       | 3.27       | 1.27       | 1.25       | 1.88       | 1.31       | 5.4        |
mandelbrot.rb       | 1.0        | 0.94       | 1.21       | 1.81       | 1.85       | 1.08       | 0.9        | 1.26       | 1.61       | 29.59      |
meteor.rb           | 1.0        | 1.22       | 1.33       | 1.3        | 1.24       | 1.15       | 0.75       | 0.74       | 0.25       | 0.35       |
nbody.rb            | 1.0        | 1.02       | 1.29       | 2.24       | 2.35       | 1.25       | 0.83       | 1.64       | 1.3        | 19.41      |
norm.rb             | 1.0        | 1.14       | 1.27       | 2.29       | 2.21       | 1.15       | 0.79       | 1.17       | 1.02       | 9.57       |
trees.rb            | 1.0        | 1.13       | 1.33       | 2.08       | 1.93       | 1.21       | 1.02       | 1.05       | 0.4        | 1.59       |
pent.rb             | 1.0        | 1.12       | 1.34       | 1.37       | 1.32       | 1.13       | 0.44       | 0.5        | 0.15       | 0.46       |
red-black.rb        | 1.0        | 0.99       | 0.99       | 0.75       | 0.67       | 0.89       | 0.67       | 1.1        | 0.55       | 2.67       |
bench.rb            | 1.0        | 1.15       | 1.22       | 1.33       | 1.44       | 1.14       | 0.96       | 1.75       | 1.07       | 4.49       |
GeoMean.            | 1.0        | 1.12       | 1.64       | 3.9        | 3.64       | 1.1        | 1.33       | 1.87       | 1.53       | 26.35      |

---

* Peak memory increase ('max resident memory' / 'max resident CRuby v2.0 memory')
    * Memory consumed by MJIT GCC or LLVM (data and code) is included
    * JITs require much more memory than the interpreter
    * OMR is the best between the JITs
    * JRuby and Graal are the worst with memory consumption

---

|                   | v2         | base       | rtl        | mjit       | mjit-cl    | omr        | jruby9k    | jruby9k-d  | graal-31   | graal-31-n |
:-------------------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
while.rb            | 1.0        | 1.02       | 1.11       | 4.62       | 8.72       | 2.6        | 466.68     | 478.08     | 58.35      | 23.23      |
nest-while.rb       | 1.0        | 1.03       | 1.11       | 5.04       | 8.74       | 2.57       | 264.4      | 354.66     | 66.08      | 25.69      |
nest-ntimes.rb      | 1.0        | 1.0        | 1.11       | 4.55       | 8.43       | 3.24       | 179.4      | 177.49     | 72.85      | 57.98      |
ivread.rb           | 1.0        | 1.0        | 1.09       | 4.44       | 8.34       | 2.5        | 410.19     | 458.32     | 58.01      | 22.41      |
ivwrite.rb          | 1.0        | 1.03       | 1.13       | 4.64       | 8.72       | 2.56       | 395.31     | 352.41     | 66.7       | 24.14      |
aread.rb            | 1.0        | 1.0        | 1.08       | 4.65       | 8.52       | 2.52       | 242.83     | 446.54     | 64.71      | 26.91      |
awrite.rb           | 1.0        | 0.98       | 1.1        | 4.57       | 8.44       | 2.5        | 293.19     | 396.48     | 65.01      | 29.0       |
aref.rb             | 1.0        | 1.0        | 1.09       | 4.48       | 8.39       | 2.47       | 298.12     | 385.44     | 63.99      | 28.1       |
aset.rb             | 1.0        | 1.02       | 1.07       | 4.57       | 8.47       | 2.5        | 321.61     | 352.65     | 64.94      | 29.58      |
const.rb            | 1.0        | 1.02       | 1.15       | 4.65       | 8.62       | 2.61       | 476.84     | 477.87     | 65.76      | 23.09      |
const2.rb           | 1.0        | 0.99       | 1.09       | 4.47       | 8.38       | 2.47       | 458.77     | 458.66     | 64.63      | 22.2       |
call.rb             | 1.0        | 1.0        | 1.09       | 4.67       | 8.41       | 3.25       | 194.13     | 423.14     | 63.5       | 29.97      |
fib.rb              | 1.0        | 1.0        | 1.08       | 4.77       | 8.47       | 3.19       | 38.13      | 37.93      | 198.61     | 45.98      |
fannk.rb            | 1.0        | 1.03       | 1.09       | 3.97       | 7.82       | 2.59       | 138.01     | 166.75     | 291.16     | 179.46     |
sieve.rb            | 1.0        | 1.03       | 1.03       | 1.03       | 1.03       | 1.05       | 15.93      | 15.98      | 4.96       | 8.25       |
mandelbrot.rb       | 1.0        | 0.96       | 1.04       | 6.82       | 8.78       | 3.2        | 269.45     | 342.19     | 64.04      | 44.33      |
meteor.rb           | 1.0        | 1.0        | 1.1        | 5.51       | 8.01       | 2.96       | 153.44     | 236.1      | 448.99     | 269.28     |
nbody.rb            | 1.0        | 1.0        | 1.09       | 7.66       | 9.43       | 3.42       | 209.17     | 312.57     | 71.1       | 40.43      |
norm.rb             | 1.0        | 1.05       | 1.12       | 5.27       | 8.38       | 3.19       | 231.09     | 332.64     | 135.12     | 178.58     |
trees.rb            | 1.0        | 0.63       | 0.65       | 0.8        | 0.65       | 1.35       | 12.32      | 14.01      | 14.29      | 10.7       |
pent.rb             | 1.0        | 0.98       | 1.09       | 5.93       | 8.87       | 3.22       | 126.74     | 146.3      | 267.62     | 209.08     |
red-black.rb        | 1.0        | 0.99       | 1.0        | 1.0        | 1.0        | 1.31       | 2.26       | 2.54       | 3.86       | 5.33       |
bench.rb            | 1.0        | 0.98       | 1.07       | 10.37      | 10.27      | 3.49       | 199.33     | 237.48     | 174.79     | 178.89     |
GeoMean.            | 1.0        | 0.98       | 1.06       | 4.11       | 6.38       | 2.54       | 150.6      | 183.37     | 67.14      | 38.13      |

---

## Optcarrot results

* Graal has the best wall time for longer running optcarrot.  All other JITs are far away from in terms of wall time performance
    * Graal results for optcarrot run with the default number of frames are not so good unless `--native` is used
    * `--native` permits to achieve the best wall time for the default number of frames but it hurts performance for a run with 2000 frames
* MJIT has better results with LLVM than with GCC
* Although JRuby produces decent FPS, it requires too much CPU resources and memory
* Optcarrot results are different from microbenchmark ones:
    * MJIT with **LLVM** produces better wall time results than with **GCC**

### 2000 frames

* Frames Per Second (Speedup = FPS / 'CRuby v2.0 FPS'):

---

|        | v2         | base       | rtl        | mjit       | mjit-cl    | omr        | jruby9k    | jruby9k-d  | graal-31   | graal-31-n |
:--------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
FPS      | 28.92      | 39.51      | 43.09      | 83.0       | 93.87      | 35.39      | 32.8       | 68.84      | 408.37     | 305.85     |
Speedup. | 1.0        | 1.37       | 1.49       | 2.87       | 3.25       | 1.22       | 1.13       | 2.38       | 14.12      | 10.58      |

---

* CPU time ('CPU CRuby v2.0 time' / 'CPU time'):

---

|        | v2         | base       | rtl        | mjit       | mjit-cl    | omr        | jruby9k    | jruby9k-d  | graal-31   | graal-31-n |
:--------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
Speedup  | 1.0        | 1.33       | 1.46       | 1.51       | 1.49       | 1.15       | 0.79       | 0.79       | 0.64       | 1.59       |

---

* Peak Memory ('max resident memory' / 'max redsident CRuby v2.0 memory'):

---

|        | v2         | base       | rtl        | mjit       | mjit-cl    | omr        | jruby9k    | jruby9k-d  | graal-31   | graal-31-n |
:--------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
Peak Mem | 1.0        | 1.0        | 1.08       | 1.15       | 1.15       | 1.42       | 9.48       | 21.41      | 32.43      | 23.4       |

### Default number of frames (180 frames)

* Frames Per Second (Speedup = FPS / 'CRuby v2.0 FPS'):

---

|        | v2         | base       | rtl        | mjit       | mjit-cl    | omr        | jruby9k    | jruby9k-d  | graal-31   | graal-31-n |
:--------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
FPS      | 29.56      | 40.09      | 43.57      | 73.37      | 84.69      | 35.75      | 33.01      | 60.39      | 45.71      | 134.36     |
Speedup. | 1.0        | 1.36       | 1.47       | 2.48       | 2.86       | 1.21       | 1.12       | 2.04       | 1.55       | 4.54       |

---

* CPU time ('CPU CRuby v2.0 time' / 'CPU time'):

---

|        | v2         | base       | rtl        | mjit       | mjit-cl    | omr        | jruby9k    | jruby9k-d  | graal-31   | graal-31-n |
:--------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
Speedup  | 1.0        | 1.27       | 1.4        | 0.92       | 0.96       | 0.92       | 0.25       | 0.13       | 0.08       | 0.21       |

---

* Peak Memory ('max resident memory' / 'max redsident CRuby v2.0 memory'):

---

|        | v2         | base       | rtl        | mjit       | mjit-cl    | omr        | jruby9k    | jruby9k-d  | graal-31   | graal-31-n |
:--------|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|-----------:|
Peak Mem | 1.0        | 0.83       | 0.94       | 0.92       | 1.03       | 1.26       | 8.21       | 9.29       | 23.37      | 21.75      |
