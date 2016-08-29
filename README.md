# Ruby hash tables with open addressing

* This is repository containing new Ruby hash tables with *open
  addressing* discussed on https://bugs.ruby-lang.org/issues/12142

* The actual code is on branch `hash_tables_with_open_addressing`.
  The branch `trunk` is Ruby trunk dated Aug. 28 on which branch
  `hash_tables_with_open_addressing` is based.

* Here is a perfromance comparison with for tables with chains for
  conflict resolution.  The average speed results are obtained on
  4.2GHz i7-4790K by running:
  

```
ruby ../ruby/benchmark/driver.rb -p hash -r 3 -e trunk::<trunk-miniruby> -e yura::<yura-miniruby> -e mine::<mine-miniruby>|awk 'NF==3 && /hash/ {s+=$2;s2+=$3;n++;print} END{print s/n, s2/n}'
```

|Table                         |Average speed up            |
:-----------------------------:|---------------------------:|
 Trunk                         | 1.0                        |
 Default tables with chains    | 1.42                       |
 Above compiled for huge tables| 1.37                       |
 Above + spihash24             | 1.36                       |
 Open addressing tables        | 1.45                       |
 

* The *tables with chains* are hash tables with chains used for
  collisions where table elements are stored in array as in the
  *tables with open addressing*.  *Trunk* tables use lists to store
  all elements and traverse them.

* *Open addressing tables* potentially permit to have tables upto 2^64
  elements on 64-bit targets

* By default, the *tables with chains* permit to use tables with less
  < 2^32 elements.

  * If you need bigger tables you should compile MRI with macro
    non-zero `ENABLE_HUGEHASH` or use a special options during MRI
    configuration.  The third column gives the results when MRI is
    compiled with this macro.

  * The code with non-zero `ENABLE_HUGEHASH` results in 33% bigger
    elements in comparison with open-addressing table elements and the
    tables with chains with elements number < 2^32.  It means that
    traversing hash table elements (a pretty frequent operation of
    Ruby code) will be never faster for hash table with chains.

  * A Ruby developer meetings decided to have tables with > 2*32
    elements potentially (as CPYTHON for example).
  
* The *tables with chains* use siphash13 (1-iteration per element and
  3-final iterations).  Trunk and *open addressing tables* use slower
  siphash24 (1-iteration per element and 3-final iteration).  To
  compare apples to apples, the third column presents results for
  *tables with chains* with siphash24.
  

# More detail results

* Trunk vs *tables with chains* and *open addressing tables*

```
bighash	2.002	1.629
hash_aref_dsym	1.001	0.976
hash_aref_dsym_long	1.467	1.438
hash_aref_fix	1.001	1.056
hash_aref_flo	1.780	1.916
hash_aref_miss	1.051	1.203
hash_aref_str	1.081	1.312
hash_aref_sym	0.980	0.991
hash_aref_sym_long	1.033	1.046
hash_flatten	1.159	1.181
hash_ident_flo	0.956	0.952
hash_ident_num	0.973	0.975
hash_ident_obj	0.933	0.967
hash_ident_str	0.967	0.979
hash_ident_sym	0.945	0.971
hash_keys	2.878	2.836
hash_long	1.205	1.566
hash_shift	1.471	1.421
hash_shift_u16	1.448	1.375
hash_shift_u24	1.415	1.366
hash_shift_u32	1.394	1.351
hash_small2	1.106	1.047
hash_small4	1.101	1.054
hash_small8	2.125	2.350
hash_to_proc	0.979	1.026
hash_values	2.859	2.823
vm2_bighash*	3.134	3.233
                1.42385 1.44593
```

* Above but tables with chains compiled to have > 2^32 elements

```
hash_aref_dsym	0.974	0.948
hash_aref_dsym_long	1.483	1.445
hash_aref_fix	1.019	1.055
hash_aref_flo	1.768	1.970
hash_aref_miss	0.987	1.167
hash_aref_str	1.092	1.355
hash_aref_sym	1.000	0.984
hash_aref_sym_long	1.031	1.044
hash_flatten	1.131	1.180
hash_ident_flo	0.979	0.989
hash_ident_num	1.007	1.005
hash_ident_obj	0.936	0.962
hash_ident_str	0.949	0.976
hash_ident_sym	0.942	0.991
hash_keys	2.545	2.791
hash_long	1.210	1.566
hash_shift	1.434	1.422
hash_shift_u16	1.468	1.396
hash_shift_u24	1.382	1.371
hash_shift_u32	1.388	1.357
hash_small2	0.997	1.048
hash_small4	1.007	1.058
hash_small8	2.000	2.359
hash_to_proc	0.978	1.026
hash_values	2.575	2.801
vm2_bighash*	2.952	3.293
                1.37219 1.44978
```

* Above plus tables with chains using siphash24

```
bighash	        1.808	1.611
hash_aref_dsym	0.977	0.954
hash_aref_dsym_long	1.465	1.426
hash_aref_fix	1.019	1.052
hash_aref_flo	1.752	1.918
hash_aref_miss	0.976	1.218
hash_aref_str	1.054	1.346
hash_aref_sym	1.015	0.989
hash_aref_sym_long	1.050	1.063
hash_flatten	1.129	1.179
hash_ident_flo	0.981	0.972
hash_ident_num	0.953	0.973
hash_ident_obj	0.907	0.934
hash_ident_str	0.977	0.969
hash_ident_sym	0.972	0.983
hash_keys	2.572	2.794
hash_long	1.004	1.564
hash_shift	1.427	1.422
hash_shift_u16	1.440	1.380
hash_shift_u24	1.396	1.371
hash_shift_u32	1.396	1.359
hash_small2	0.991	1.043
hash_small4	1.008	1.016
hash_small8	1.986	2.359
hash_to_proc	0.976	1.043
hash_values	2.593	2.798
vm2_bighash*	2.972	3.304
		1.36281 1.44593
```