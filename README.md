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

# RTL MJIT performance as Feb. 18, 2018

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
    * graal-22 - Graal Ruby version 0.22

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
    * MJIT gives a real speedup comparable with other Ruby JITs
    * OMR is currently the worst performance JIT
    * In most cases, using GCC is better choice for MJIT than LLVM

---

|                   | v2        | base      | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-22  |
:-------------------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
while.rb            | 1.0       | 1.11      | 2.29      | 12.75     | 10.17     | 1.05      | 2.33      | 2.92      | 2.39      |
nest-while.rb       | 1.0       | 1.09      | 1.84      | 4.69      | 4.53      | 1.07      | 1.62      | 2.56      | 1.7       |
nest-ntimes.rb      | 1.0       | 1.01      | 1.2       | 2.18      | 2.34      | 1.02      | 0.94      | 1.02      | 2.25      |
ivread.rb           | 1.0       | 1.13      | 2.46      | 13.67     | 10.9      | 1.14      | 2.44      | 3.01      | 2.3       |
ivwrite.rb          | 1.0       | 1.26      | 2.23      | 7.81      | 7.72      | 1.16      | 2.63      | 3.03      | 2.04      |
aread.rb            | 1.0       | 1.02      | 1.61      | 9.15      | 7.46      | 0.98      | 1.79      | 3.57      | 2.19      |
awrite.rb           | 1.0       | 1.08      | 1.69      | 8.5       | 7.76      | 0.95      | 1.97      | 3.8       | 2.58      |
aref.rb             | 1.0       | 1.1       | 2.34      | 11.62     | 10.24     | 1.11      | 1.85      | 3.69      | 3.73      |
aset.rb             | 1.0       | 1.47      | 3.36      | 14.1      | 12.76     | 1.42      | 3.54      | 4.5       | 6.22      |
const.rb            | 1.0       | 1.13      | 2.09      | 11.78     | 10.68     | 1.07      | 3.06      | 4.02      | 3.1       |
const2.rb           | 1.0       | 1.12      | 2.0       | 11.79     | 10.03     | 1.1       | 3.1       | 3.85      | 2.44      |
call.rb             | 1.0       | 1.14      | 1.81      | 4.37      | 4.33      | 0.91      | 2.2       | 5.1       | 2.9       |
fib.rb              | 1.0       | 1.19      | 1.62      | 4.09      | 3.9       | 1.11      | 2.16      | 5.11      | 2.31      |
fannk.rb            | 1.0       | 1.05      | 1.13      | 1.11      | 1.13      | 1.02      | 1.72      | 2.31      | 1.05      |
sieve.rb            | 1.0       | 1.29      | 2.02      | 3.44      | 3.31      | 1.27      | 1.49      | 2.45      | 2.06      |
mandelbrot.rb       | 1.0       | 0.93      | 1.19      | 1.85      | 1.91      | 1.07      | 0.99      | 1.59      | 2.55      |
meteor.rb           | 1.0       | 1.22      | 1.35      | 1.64      | 1.63      | 1.16      | 0.88      | 0.96      | 0.55      |
nbody.rb            | 1.0       | 1.06      | 1.3       | 2.55      | 2.71      | 1.27      | 1.02      | 2.35      | 2.18      |
norm.rb             | 1.0       | 1.13      | 1.3       | 2.48      | 2.46      | 1.14      | 0.91      | 1.44      | 1.61      |
trees.rb            | 1.0       | 1.14      | 1.33      | 2.21      | 2.08      | 1.21      | 1.4       | 1.53      | 0.79      |
pent.rb             | 1.0       | 1.13      | 1.36      | 1.67      | 1.63      | 1.13      | 0.57      | 0.74      | 0.36      |
red-black.rb        | 1.0       | 1.01      | 0.96      | 1.32      | 1.24      | 0.89      | 0.99      | 2.47      | 1.07      |
bench.rb            | 1.0       | 1.17      | 1.23      | 1.66      | 1.8       | 1.16      | 1.27      | 2.86      | 1.83      |
GeoMean.            | 1.0       | 1.13      | 1.65      | 4.23      | 4.01      | 1.1       | 1.6       | 2.5       | 1.86      |

---

* CPU time improvements ('CPU CRuby v2.0 time' / 'CPU time')
    * CPU time is important too for cloud (money) or mobile (battery)
    * MJIT almost always spends less CPU than the current CRuby interpreter
    * Graal is too aggressive with compilations and almost always needs
      more CPU work than CRuby interpreter
  
---

|                   | v2        | base      | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-22  |
:-------------------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
while.rb            | 1.0       | 1.11      | 2.29      | 12.34     | 9.76      | 1.05      | 1.99      | 2.38      | 1.01      |
nest-while.rb       | 1.0       | 1.09      | 1.84      | 4.55      | 4.36      | 1.07      | 1.35      | 1.93      | 0.7       |
nest-ntimes.rb      | 1.0       | 1.01      | 1.2       | 2.13      | 2.26      | 1.02      | 0.86      | 0.9       | 0.79      |
ivread.rb           | 1.0       | 1.13      | 2.47      | 13.23     | 10.47     | 1.14      | 2.1       | 2.45      | 0.96      |
ivwrite.rb          | 1.0       | 1.26      | 2.23      | 7.6       | 7.38      | 1.16      | 2.09      | 2.3       | 0.77      |
aread.rb            | 1.0       | 1.02      | 1.61      | 8.85      | 7.16      | 0.98      | 1.49      | 2.52      | 0.84      |
awrite.rb           | 1.0       | 1.08      | 1.69      | 8.24      | 7.47      | 0.95      | 1.65      | 2.74      | 0.98      |
aref.rb             | 1.0       | 1.1       | 2.34      | 11.37     | 9.93      | 1.11      | 1.7       | 3.1       | 1.56      |
aset.rb             | 1.0       | 1.47      | 3.36      | 13.88     | 12.5      | 1.42      | 3.21      | 3.88      | 2.6       |
const.rb            | 1.0       | 1.13      | 2.09      | 11.65     | 10.51     | 1.07      | 2.8       | 3.59      | 1.78      |
const2.rb           | 1.0       | 1.12      | 2.0       | 11.66     | 9.87      | 1.1       | 2.85      | 3.44      | 1.52      |
call.rb             | 1.0       | 1.14      | 1.81      | 4.31      | 4.22      | 0.91      | 1.83      | 3.56      | 1.05      |
fib.rb              | 1.0       | 1.2       | 1.62      | 4.03      | 3.83      | 1.11      | 1.84      | 3.62      | 0.91      |
fannk.rb            | 1.0       | 1.05      | 1.13      | 1.11      | 1.13      | 1.02      | 1.46      | 1.77      | 0.47      |
sieve.rb            | 1.0       | 1.29      | 2.03      | 3.42      | 3.27      | 1.27      | 1.25      | 1.89      | 0.79      |
mandelbrot.rb       | 1.0       | 0.93      | 1.19      | 1.77      | 1.82      | 1.07      | 0.86      | 1.35      | 0.81      |
meteor.rb           | 1.0       | 1.22      | 1.35      | 1.31      | 1.25      | 1.16      | 0.74      | 0.76      | 0.17      |
nbody.rb            | 1.0       | 1.06      | 1.3       | 2.31      | 2.43      | 1.27      | 0.86      | 1.75      | 0.73      |
norm.rb             | 1.0       | 1.13      | 1.3       | 2.28      | 2.22      | 1.14      | 0.8       | 1.19      | 0.57      |
trees.rb            | 1.0       | 1.14      | 1.33      | 2.07      | 1.93      | 1.21      | 1.0       | 1.03      | 0.26      |
pent.rb             | 1.0       | 1.13      | 1.36      | 1.4       | 1.32      | 1.13      | 0.44      | 0.51      | 0.1       |
red-black.rb        | 1.0       | 1.01      | 0.96      | 0.76      | 0.68      | 0.89      | 0.68      | 1.12      | 0.31      |
bench.rb            | 1.0       | 1.17      | 1.23      | 1.35      | 1.43      | 1.16      | 0.94      | 1.8       | 0.52      |
GeoMean.            | 1.0       | 1.13      | 1.65      | 3.92      | 3.66      | 1.1       | 1.34      | 1.89      | 0.7       |

---

* Peak memory increase ('max resident memory' / 'max resident CRuby v2.0 memory')
    * Memory consumed by MJIT GCC or LLVM (data and code) is included
    * JITs require much more memory than the interpreter
    * OMR is the best between the JITs
    * JRuby and Graal are the worst with memory consumption

---

|                   | v2        | base      | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-22  |
:-------------------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
while.rb            | 1.0       | 1.0       | 1.08      | 4.49      | 8.29      | 2.48      | 414.21    | 459.46    | 116.08    |
nest-while.rb       | 1.0       | 1.04      | 1.12      | 5.02      | 8.77      | 2.54      | 238.71    | 307.83    | 96.54     |
nest-ntimes.rb      | 1.0       | 0.99      | 1.1       | 4.55      | 8.41      | 3.25      | 164.47    | 172.87    | 120.63    |
ivread.rb           | 1.0       | 0.99      | 1.1       | 4.44      | 8.32      | 2.5       | 457.36    | 457.44    | 106.72    |
ivwrite.rb          | 1.0       | 0.95      | 1.09      | 4.41      | 8.26      | 2.47      | 348.4     | 406.52    | 100.0     |
aread.rb            | 1.0       | 1.01      | 1.08      | 4.63      | 8.43      | 2.52      | 226.01    | 370.36    | 95.88     |
awrite.rb           | 1.0       | 0.98      | 1.08      | 4.46      | 8.25      | 2.45      | 265.13    | 433.91    | 92.8      |
aref.rb             | 1.0       | 0.98      | 1.06      | 4.44      | 8.27      | 2.43      | 299.87    | 452.81    | 116.1     |
aset.rb             | 1.0       | 0.98      | 1.08      | 4.49      | 8.42      | 2.47      | 289.16    | 382.99    | 94.42     |
const.rb            | 1.0       | 0.99      | 1.1       | 4.51      | 8.56      | 2.51      | 467.33    | 467.48    | 103.94    |
const2.rb           | 1.0       | 1.0       | 1.11      | 4.49      | 8.47      | 2.52      | 463.82    | 463.45    | 93.72     |
call.rb             | 1.0       | 1.04      | 1.13      | 4.84      | 8.74      | 3.37      | 253.45    | 408.67    | 104.08    |
fib.rb              | 1.0       | 1.02      | 1.11      | 4.83      | 8.48      | 3.23      | 38.41     | 38.46     | 148.84    |
fannk.rb            | 1.0       | 1.02      | 1.06      | 3.91      | 7.68      | 2.49      | 185.14    | 150.81    | 149.5     |
sieve.rb            | 1.0       | 1.03      | 1.03      | 1.03      | 1.03      | 1.05      | 14.9      | 16.04     | 5.89      |
mandelbrot.rb       | 1.0       | 0.98      | 1.03      | 6.93      | 8.94      | 3.23      | 277.47    | 362.41    | 98.33     |
meteor.rb           | 1.0       | 0.99      | 1.07      | 5.49      | 7.88      | 2.9       | 184.35    | 159.1     | 163.39    |
nbody.rb            | 1.0       | 0.99      | 1.09      | 7.61      | 9.34      | 3.4       | 205.04    | 416.44    | 99.32     |
norm.rb             | 1.0       | 1.0       | 1.1       | 5.19      | 8.22      | 3.15      | 259.45    | 334.28    | 142.84    |
trees.rb            | 1.0       | 0.63      | 0.65      | 0.8       | 0.65      | 1.35      | 11.55     | 14.22     | 8.04      |
pent.rb             | 1.0       | 0.99      | 1.07      | 5.81      | 8.77      | 3.17      | 111.19    | 152.62    | 147.79    |
red-black.rb        | 1.0       | 0.99      | 1.0       | 1.0       | 1.0       | 1.31      | 2.41      | 2.56      | 3.96      |
bench.rb            | 1.0       | 0.99      | 1.1       | 10.37     | 10.29     | 3.49      | 192.24    | 231.49    | 153.08    |

---

## Optcarrot results

* Graal crashes with `convert_type` exception
* MJIT with LLVM has the best results
* Although JRuby produces decent FPS, it requires too much CPU resources and memory
* Optcarrot results are different from microbenchmark ones:
    * MJIT with **LLVM** produces better wall time results than with **GCC**

### 2000 frames

* Frames Per Second (Speedup = FPS / 'CRuby v2.0 FPS'):

---

|        | v2        | base      | old-rtl   | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-22  |
:--------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
FPS      | 29.18     | 39.83     | 34.65     | 42.59     | 82.24     | 91.4      | 35.13     | 33.48     | 68.89     | -         |
Speedup. | 1.0       | 1.36      | 1.19      | 1.46      | 2.82      | 3.13      | 1.2       | 1.15      | 2.36      | -         |

---

* CPU time ('CPU CRuby v2.0 time' / 'CPU time'):

---

|        | v2        | base      | old-rtl   | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-22  |
:--------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
Speedup  | 1.0       | 1.33      | 1.13      | 1.37      | 1.48      | 1.45      | 1.13      | 0.8       | 0.77      | -         |

---

* Peak Memory ('max resident memory' / 'max redsident CRuby v2.0 memory'):

---

|        | v2        | base      | old-rtl   | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-22  |
:--------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
Peak Mem | 1.0       | 1.0       | 1.1       | 1.08      | 1.15      | 1.16      | 1.42      | 11.07     | 15.71     | -         |

### Default number of frames (180 frames)

* Frames Per Second (Speedup = FPS / 'CRuby v2.0 FPS'):

---

|        | v2        | base      | old-rtl   | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-22  |
:--------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
FPS      | 29.29     | 39.57     | 35.41     | 43.18     | 80.82     | 93.26     | 35.21     | 32.63     | 69.36     | -         |
Speedup. | 1.0       | 1.35      | 1.21      | 1.47      | 2.76      | 3.18      | 1.2       | 1.11      | 2.37      | -         |

---

* CPU time ('CPU CRuby v2.0 time' / 'CPU time'):

---

|        | v2        | base      | old-rtl   | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-22  |
:--------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
Speedup  | 1.0       | 1.32      | 1.18      | 1.39      | 1.47      | 1.46      | 1.13      | 0.78      | 0.77      | -         |

---

* Peak Memory ('max resident memory' / 'max redsident CRuby v2.0 memory'):

---

|        | v2        | base      | old-rtl   | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-22  |
:--------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
Peak Mem | 1.0       | 1.0       | 1.1       | 1.09      | 1.15      | 1.15      | 1.41      | 9.2       | 15.43     | -         |
