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

|                   | v2        | base      | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-31  |
:-------------------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
while.rb            | 1.0       | 1.1       | 2.28      | 12.65     | 10.09     | 1.05      | 2.28      | 2.81      | 9.26      |
nest-while.rb       | 1.0       | 1.09      | 1.8       | 4.61      | 4.46      | 1.05      | 1.36      | 2.6       | 5.73      |
nest-ntimes.rb      | 1.0       | 1.01      | 1.17      | 2.12      | 2.35      | 1.0       | 0.93      | 0.97      | 5.53      |
ivread.rb           | 1.0       | 1.14      | 2.47      | 13.67     | 10.9      | 1.13      | 2.41      | 3.03      | 9.61      |
ivwrite.rb          | 1.0       | 1.23      | 2.19      | 7.72      | 7.6       | 1.13      | 2.55      | 2.99      | 6.59      |
aread.rb            | 1.0       | 1.01      | 1.61      | 8.85      | 7.22      | 0.95      | 1.76      | 3.45      | 7.05      |
awrite.rb           | 1.0       | 1.08      | 1.69      | 8.51      | 7.76      | 0.96      | 1.98      | 3.77      | 8.04      |
aref.rb             | 1.0       | 1.13      | 2.34      | 11.69     | 10.25     | 1.1       | 1.81      | 3.72      | 13.77     |
aset.rb             | 1.0       | 1.46      | 3.43      | 14.08     | 12.79     | 1.44      | 3.53      | 4.42      | 22.67     |
const.rb            | 1.0       | 1.08      | 2.0       | 11.48     | 10.45     | 1.07      | 3.0       | 3.92      | 21.72     |
const2.rb           | 1.0       | 1.13      | 2.11      | 11.9      | 10.15     | 1.11      | 3.1       | 3.88      | 22.16     |
call.rb             | 1.0       | 1.14      | 1.77      | 4.29      | 4.29      | 0.91      | 2.14      | 4.95      | 8.52      |
fib.rb              | 1.0       | 1.21      | 1.65      | 4.23      | 3.86      | 1.11      | 2.2       | 5.06      | 5.4       |
fannk.rb            | 1.0       | 1.05      | 1.12      | 1.11      | 1.12      | 1.01      | 1.68      | 2.3       | 2.48      |
sieve.rb            | 1.0       | 1.3       | 2.03      | 3.45      | 3.31      | 1.28      | 1.51      | 2.43      | 3.74      |
mandelbrot.rb       | 1.0       | 0.94      | 1.21      | 1.87      | 1.9       | 1.07      | 0.99      | 1.56      | 5.76      |
meteor.rb           | 1.0       | 1.23      | 1.34      | 1.64      | 1.64      | 1.16      | 0.9       | 0.95      | 0.86      |
nbody.rb            | 1.0       | 1.04      | 1.3       | 2.44      | 2.61      | 1.24      | 0.97      | 2.29      | 4.5       |
norm.rb             | 1.0       | 1.13      | 1.3       | 2.5       | 2.46      | 1.15      | 0.92      | 1.44      | 3.47      |
trees.rb            | 1.0       | 1.16      | 1.35      | 2.25      | 2.13      | 1.23      | 1.43      | 1.54      | 1.32      |
pent.rb             | 1.0       | 1.13      | 1.35      | 1.66      | 1.65      | 1.14      | 0.58      | 0.74      | 0.57      |
red-black.rb        | 1.0       | 0.98      | 0.96      | 1.29      | 1.3       | 0.87      | 1.1       | 2.5       | 1.83      |
bench.rb            | 1.0       | 1.16      | 1.27      | 1.66      | 1.78      | 1.16      | 1.28      | 2.76      | 3.63      |
GeoMean.            | 1.0       | 1.12      | 1.65      | 4.21      | 3.99      | 1.09      | 1.59      | 2.48      | 5.17      |

---

* CPU time improvements ('CPU CRuby v2.0 time' / 'CPU time')
    * CPU time is important too for cloud (money) or mobile (battery)
    * MJIT almost always spends less CPU than the current CRuby interpreter
    * Graal is too aggressive with compilations and needs sometimes
      more CPU work than CRuby interpreter
  
---

|                   | v2        | base      | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-31  |
:-------------------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
while.rb            | 1.0       | 1.1       | 2.28      | 12.25     | 9.69      | 1.05      | 1.98      | 2.28      | 2.63      |
nest-while.rb       | 1.0       | 1.09      | 1.79      | 4.47      | 4.27      | 1.05      | 1.14      | 1.96      | 1.61      |
nest-ntimes.rb      | 1.0       | 1.01      | 1.17      | 2.07      | 2.26      | 1.0       | 0.84      | 0.87      | 1.54      |
ivread.rb           | 1.0       | 1.14      | 2.47      | 13.16     | 10.5      | 1.13      | 2.05      | 2.45      | 2.74      |
ivwrite.rb          | 1.0       | 1.24      | 2.19      | 7.51      | 7.27      | 1.13      | 2.0       | 2.27      | 1.88      |
aread.rb            | 1.0       | 1.01      | 1.61      | 8.55      | 6.95      | 0.95      | 1.47      | 2.41      | 2.02      |
awrite.rb           | 1.0       | 1.08      | 1.69      | 8.3       | 7.49      | 0.96      | 1.65      | 2.83      | 2.3       |
aref.rb             | 1.0       | 1.13      | 2.34      | 11.46     | 9.98      | 1.1       | 1.65      | 3.12      | 3.92      |
aset.rb             | 1.0       | 1.46      | 3.43      | 13.88     | 12.5      | 1.44      | 3.2       | 3.87      | 6.42      |
const.rb            | 1.0       | 1.08      | 2.0       | 11.33     | 10.27     | 1.07      | 2.76      | 3.49      | 6.21      |
const2.rb           | 1.0       | 1.13      | 2.11      | 11.75     | 9.99      | 1.11      | 2.82      | 3.48      | 6.31      |
call.rb             | 1.0       | 1.14      | 1.77      | 4.21      | 4.18      | 0.91      | 1.79      | 3.48      | 2.41      |
fib.rb              | 1.0       | 1.21      | 1.65      | 4.18      | 3.79      | 1.11      | 1.88      | 3.53      | 1.83      |
fannk.rb            | 1.0       | 1.05      | 1.12      | 1.11      | 1.11      | 1.01      | 1.43      | 1.77      | 1.03      |
sieve.rb            | 1.0       | 1.3       | 2.04      | 3.43      | 3.27      | 1.27      | 1.27      | 1.84      | 1.33      |
mandelbrot.rb       | 1.0       | 0.94      | 1.21      | 1.8       | 1.82      | 1.07      | 0.88      | 1.3       | 1.61      |
meteor.rb           | 1.0       | 1.23      | 1.34      | 1.3       | 1.25      | 1.16      | 0.75      | 0.75      | 0.26      |
nbody.rb            | 1.0       | 1.04      | 1.3       | 2.22      | 2.35      | 1.24      | 0.82      | 1.68      | 1.34      |
norm.rb             | 1.0       | 1.13      | 1.3       | 2.3       | 2.21      | 1.15      | 0.8       | 1.2       | 1.0       |
trees.rb            | 1.0       | 1.16      | 1.35      | 2.11      | 1.97      | 1.23      | 1.03      | 1.05      | 0.41      |
pent.rb             | 1.0       | 1.13      | 1.34      | 1.38      | 1.33      | 1.14      | 0.45      | 0.51      | 0.15      |
red-black.rb        | 1.0       | 0.98      | 0.96      | 0.74      | 0.74      | 0.87      | 0.73      | 1.15      | 0.52      |
bench.rb            | 1.0       | 1.16      | 1.27      | 1.35      | 1.42      | 1.16      | 0.95      | 1.71      | 1.04      |
GeoMean.            | 1.0       | 1.12      | 1.65      | 3.9       | 3.66      | 1.09      | 1.33      | 1.87      | 1.53      |

---

* Peak memory increase ('max resident memory' / 'max resident CRuby v2.0 memory')
    * Memory consumed by MJIT GCC or LLVM (data and code) is included
    * JITs require much more memory than the interpreter
    * OMR is the best between the JITs
    * JRuby and Graal are the worst with memory consumption

---

|                   | v2        | base      | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-31  |
:-------------------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
while.rb            | 1.0       | 1.0       | 1.11      | 4.42      | 8.37      | 2.49      | 458.61    | 458.83    | 57.89     |
nest-while.rb       | 1.0       | 0.98      | 1.1       | 4.91      | 8.48      | 2.5       | 283.75    | 356.31    | 64.3      |
nest-ntimes.rb      | 1.0       | 0.99      | 1.09      | 4.56      | 8.43      | 3.27      | 162.5     | 179.3     | 72.88     |
ivread.rb           | 1.0       | 1.0       | 1.09      | 4.47      | 8.37      | 2.5       | 460.82    | 460.75    | 64.39     |
ivwrite.rb          | 1.0       | 0.99      | 1.07      | 4.38      | 8.28      | 2.47      | 405.21    | 398.96    | 63.16     |
aread.rb            | 1.0       | 0.99      | 1.1       | 4.55      | 8.36      | 2.48      | 265.75    | 414.98    | 63.82     |
awrite.rb           | 1.0       | 1.01      | 1.07      | 4.51      | 8.45      | 2.51      | 248.21    | 423.73    | 64.95     |
aref.rb             | 1.0       | 0.98      | 1.08      | 4.5       | 8.38      | 2.48      | 307.73    | 436.22    | 64.0      |
aset.rb             | 1.0       | 1.0       | 1.11      | 4.53      | 8.48      | 2.55      | 324.51    | 358.45    | 67.87     |
const.rb            | 1.0       | 1.01      | 1.11      | 4.48      | 8.48      | 2.55      | 465.36    | 466.11    | 65.31     |
const2.rb           | 1.0       | 1.02      | 1.06      | 4.46      | 8.39      | 2.47      | 442.06    | 461.06    | 65.37     |
call.rb             | 1.0       | 1.03      | 1.13      | 4.84      | 8.78      | 3.39      | 231.62    | 449.97    | 62.59     |
fib.rb              | 1.0       | 1.01      | 1.08      | 4.77      | 8.45      | 3.18      | 37.4      | 37.98     | 197.19    |
fannk.rb            | 1.0       | 1.03      | 1.13      | 4.05      | 8.03      | 2.58      | 183.68    | 173.82    | 303.11    |
sieve.rb            | 1.0       | 1.03      | 1.03      | 1.03      | 1.03      | 1.05      | 16.07     | 16.08     | 4.61      |
mandelbrot.rb       | 1.0       | 0.96      | 1.04      | 6.81      | 8.81      | 3.18      | 251.79    | 309.32    | 63.36     |
meteor.rb           | 1.0       | 0.98      | 1.08      | 5.49      | 7.9       | 2.94      | 155.58    | 156.79    | 437.89    |
nbody.rb            | 1.0       | 0.99      | 1.04      | 7.53      | 9.21      | 3.36      | 264.11    | 358.93    | 64.8      |
norm.rb             | 1.0       | 1.04      | 1.12      | 5.23      | 8.37      | 3.19      | 239.75    | 366.83    | 142.08    |
trees.rb            | 1.0       | 0.63      | 0.65      | 0.8       | 0.65      | 1.35      | 12.58     | 14.88     | 14.71     |
pent.rb             | 1.0       | 0.99      | 1.06      | 5.96      | 9.03      | 3.26      | 132.63    | 154.45    | 292.94    |
red-black.rb        | 1.0       | 0.99      | 1.0       | 1.0       | 1.0       | 1.31      | 2.28      | 2.55      | 4.15      |
bench.rb            | 1.0       | 1.0       | 1.09      | 10.47     | 10.36     | 3.51      | 201.75    | 302.79    | 182.22    |
GeoMean.            | 1.0       | 0.98      | 1.06      | 4.09      | 6.35      | 2.53      | 155.59    | 186.71    | 67.58     |

---

## Optcarrot results

* Graal has the best results for longer running optcarrot.  All other JITs are far away from in terms of wall time performance
    * Graal results for optcarrot run with standard number of frames are not so good
* MJIT has better results with LLVM than with GCC
* Although JRuby produces decent FPS, it requires too much CPU resources and memory
* Optcarrot results are different from microbenchmark ones:
    * MJIT with **LLVM** produces better wall time results than with **GCC**

### 2000 frames

* Frames Per Second (Speedup = FPS / 'CRuby v2.0 FPS'):

---

|        | v2        | base      | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-31  |
:--------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
FPS      | 29.28     | 39.58     | 43.0      | 80.79     | 92.9      | 34.76     | 32.12     | 69.39     | 407.47    |
Speedup. | 1.0       | 1.35      | 1.47      | 2.76      | 3.17      | 1.19      | 1.1       | 2.37      | 13.92     |

---

* CPU time ('CPU CRuby v2.0 time' / 'CPU time'):

---

|        | v2        | base      | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-31  |
:--------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
Speedup  | 1.0       | 1.32      | 1.43      | 1.46      | 1.46      | 1.12      | 0.79      | 0.76      | 0.59      |

---

* Peak Memory ('max resident memory' / 'max redsident CRuby v2.0 memory'):

---

|        | v2        | base      | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-31  |
:--------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
Peak Mem | 1.0       | 1.0       | 1.08      | 1.15      | 1.15      | 1.41      | 10.96     | 14.93     | 33.98     |

### Default number of frames (180 frames)

* Frames Per Second (Speedup = FPS / 'CRuby v2.0 FPS'):

---

|        | v2        | base      | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-31  |
:--------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
FPS      | 30.04     | 39.83     | 43.93     | 68.16     | 84.59     | 35.89     | 33.3      | 57.62     | 49.31     |
Speedup. | 1.0       | 1.33      | 1.46      | 2.27      | 2.82      | 1.19      | 1.11      | 1.92      | 1.64      |

---

* CPU time ('CPU CRuby v2.0 time' / 'CPU time'):

---

|        | v2        | base      | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-31  |
:--------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
Speedup  | 1.0       | 1.27      | 1.39      | 0.85      | 0.96      | 0.91      | 0.23      | 0.13      | 0.07      |

---

* Peak Memory ('max resident memory' / 'max redsident CRuby v2.0 memory'):

---

|        | v2        | base      | rtl       | mjit      | mjit-cl   | omr       | jruby9k   | jruby9k-d | graal-31  |
:--------|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|----------:|
Peak Mem | 1.0       | 0.83      | 0.9       | 0.94      | 1.03      | 1.26      | 8.7       | 10.41     | 27.6      |
