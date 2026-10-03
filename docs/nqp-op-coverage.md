# `nqp::` op coverage

Which of the `nqp::` ops a Raku program can call does mutsu implement? This is
the inventory the `nqp::` coverage campaign
([#11488](https://github.com/tokuhirom/mutsu/issues/11488)) is tracked against.
The campaign replaces the 2026-07 "add `nqp::` ops on demand only" rule
(`news/2026-07/nqp-op-layer-measured-and-rejected.md`): per distribution the
op set is a threshold function, so the whole documented set is closed category
by category instead of one op per failing dist.

**Measured 2026-10-03** with

```sh
scripts/nqp-op-coverage.py --mutsu target/debug/mutsu > table.md
```

- **Universe**: NQP's op reference
  ([`docs/ops.markdown`](https://github.com/Raku/nqp/blob/main/docs/ops.markdown))
  plus the Rakudo-only ops registered in Rakudo's
  [`src/vm/moar/Perl6/Ops.nqp`](https://github.com/rakudo/rakudo/blob/main/src/vm/moar/Perl6/Ops.nqp).
- **Implemented** means `use nqp; nqp::<op>(1, ...)` with some arity from 0 to
  5 does not die with `Unsupported nqp:: op`. This is a coverage probe, not a
  correctness test: an implemented op can still give a wrong answer.
- **Out of scope**: JS/JVM-only ops (mutsu emulates the MoarVM backend), and
  ops Rakudo itself rejects ("No registered operation handler" / "Unknown
  constant") — no Raku program can reach them.

When a PR lands ops, re-run the script and replace the table below in the same
PR. An op that turns out to have no meaning in mutsu (for example a
serialization-context op, with no precompiled object graph to serialize) is
recorded under "Not applicable" with its reason, never stubbed.

## Coverage

| Category | Implemented | Missing | Tracking |
| --- | ---: | ---: | --- |
| Arithmetic | 22 / 26 | 4 | #11490 |
| Array | 34 / 61 | 27 | #11493 |
| Asynchronous | 0 / 11 | 11 | #11502 |
| Atomic | 0 / 11 | 11 | #11502 |
| Binary Data | 6 / 6 | 0 |  |
| Bit | 12 / 15 | 3 | #11491 |
| Captures | 0 / 5 | 5 | #11496 |
| Coercion | 2 / 10 | 8 | #11492 |
| Conditional | 3 / 5 | 2 | #11500 |
| Context Introspection | 4 / 24 | 20 | #11498 |
| Loop/Control | 5 / 7 | 2 | #11500 |
| Exception Handling | 3 / 15 | 12 | #11497 |
| Processes | 0 / 4 | 4 | #11501 |
| File / Directory / Network | 9 / 25 | 16 | #11501 |
| Hash | 5 / 15 | 10 | #11494 |
| HLL-Specific | 3 / 13 | 10 | #11504 |
| Input/Output | 6 / 14 | 8 | #11501 |
| Relational / Logic | 27 / 40 | 13 | #11491 |
| NativeCall | 6 / 7 | 1 | #11504 |
| Numeric | 1 / 17 | 16 | #11490 |
| Objects | 17 / 31 | 14 | #11499 |
| Parametric Extensions | 0 / 5 | 5 | #11499 |
| Profiling | 0 / 3 | 3 | #11504 |
| Serialization context | 1 / 18 | 17 | #11504 |
| Stream Decoding | 0 / 10 | 10 | #11503 |
| String | 25 / 48 | 23 | #11495 |
| System Introspection | 18 / 29 | 11 | #11501 |
| Threads | 0 / 7 | 7 | #11502 |
| Timish | 1 / 3 | 2 | #11501 |
| Trigonometric | 0 / 10 | 10 | #11490 |
| Type / Conversion | 21 / 53 | 32 | #11492 |
| Unicode Properties | 3 / 8 | 5 | #11495 |
| Miscellaneous | 1 / 4 | 3 | #11499 |
| Rakudo p6* (HLL) | 1 / 26 | 25 | #11505 |
| **Total** | **236 / 586** | **350** | |

## Missing ops by category

- **Arithmetic** (#11490): `div_In`, `gcd_i`, `lcm_i`, `mod_n`
- **Array** (#11493): `atpos2d`, `atpos2d_i`, `atpos2d_n`, `atpos2d_s`, `atpos3d`, `atpos3d_i`, `atpos3d_n`, `atpos3d_s`, `atposnd`, `atposnd_i`, `atposnd_n`, `atposnd_s`, `atposref_s`, `bindpos2d`, `bindpos2d_i`, `bindpos2d_n`, `bindpos2d_s`, `bindpos3d`, `bindpos3d_i`, `bindpos3d_n`, `bindpos3d_s`, `bindposnd`, `bindposnd_i`, `bindposnd_n`, `bindposnd_s`, `existspos`, `list_b`
- **Asynchronous** (#11502): `asyncconnect`, `asynclisten`, `asyncreadbytes`, `asyncwritebytes`, `cancel`, `killprocasync`, `permit`, `signal`, `spawnprocasync`, `timer`, `watchfile`
- **Atomic** (#11502): `atomicadd_i`, `atomicbindattr`, `atomicdec_i`, `atomicinc_i`, `atomicload`, `atomicload_i`, `atomicstore`, `atomicstore_i`, `barrierfull`, `cas`, `cas_i`
- **Bit** (#11491): `bitand_s`, `bitor_s`, `bitxor_s`
- **Captures** (#11496): `captureexistsnamed`, `capturehasnameds`, `captureposelems`, `savecapture`, `usecapture`
- **Coercion** (#11492): `coerce_in`, `coerce_iu`, `coerce_ni`, `coerce_ns`, `coerce_ui`, `coerce_us`, `intify`, `numify`
- **Conditional** (#11500): `with`, `without`
- **Context Introspection** (#11498): `bindlex`, `bindlex_i`, `bindlex_n`, `bindlex_s`, `bindlexdyn`, `ctxouter`, `curlexpad`, `getlex`, `getlex_i`, `getlex_n`, `getlex_s`, `getlexcaller`, `getlexouter`, `getlexref_i`, `getlexref_n`, `getlexref_s`, `getlexrel`, `getlexrelcaller`, `getlexreldyn`, `lexprimspec`
- **Loop/Control** (#11500): `defor`, `for`
- **Exception Handling** (#11497): `backtracestrings`, `die`, `die_s`, `exception`, `getextype`, `newexception`, `resume`, `rethrow`, `setextype`, `setmessage`, `setpayload`, `throw`
- **Processes** (#11501): `execname`, `exit`, `getpid`, `getppid`
- **File / Directory / Network** (#11501): `chdir`, `chmod`, `chown`, `copy`, `cwd`, `fileexecutable`, `filewritable`, `getport`, `link`, `lstat_time`, `mkdir`, `rename`, `rmdir`, `stat_time`, `symlink`, `unlink`
- **Hash** (#11494): `atkey_i`, `atkey_n`, `atkey_s`, `atkey_u`, `bindkey_i`, `bindkey_n`, `bindkey_s`, `iterator`, `iterkey_s`, `iterval`
- **HLL-Specific** (#11504): `bindcurhllsym`, `getcurhllsym`, `hllboxtype_i`, `hllboxtype_n`, `hllboxtype_s`, `hllhash`, `hlllist`, `sethllconfig`, `usecompileehllconfig`, `usecompilerhllconfig`
- **Input/Output** (#11501): `eoffh`, `filenofh`, `flushfh`, `print`, `say`, `seekfh`, `tellfh`, `writefh`
- **Relational / Logic** (#11491): `cmp_u`, `eqaticim`, `eqatim`, `iseq_u`, `isge_s`, `isge_u`, `isgt_s`, `isgt_u`, `isle_s`, `isle_u`, `islt_s`, `islt_u`, `isne_u`
- **NativeCall** (#11504): `nativecallinvoke`
- **Numeric** (#11490): `base_I`, `ceil_n`, `exp_n`, `expmod_I`, `floor_n`, `inf`, `log_n`, `nan`, `neginf`, `pow_i`, `pow_n`, `rand_n`, `rand_i`, `rand_I`, `sqrt_n`, `srand`
- **Objects** (#11499): `bind`, `bindcomp`, `call`, `callmethod`, `findmethod`, `how`, `how_nd`, `objectid`, `rebless`, `reprname`, `setwho`, `tryfindmethod`, `what_nd`, `who`
- **Parametric Extensions** (#11499): `setparameterizer`, `parameterizetype`, `typeparameterat`, `typeparameterized`, `typeparameters`
- **Profiling** (#11504): `force_gc`, `mvmendprofile`, `mvmstartprofile`
- **Serialization context** (#11504): `createsc`, `deserialize`, `forceouterctx`, `freshcoderef`, `getobjsc`, `markcodestatic`, `popcompsc`, `pushcompsc`, `scgetdesc`, `scgethandle`, `scgetobjidx`, `scobjcount`, `scsetcode`, `scsetdesc`, `scsetobj`, `serialize`, `setobjsc`
- **Stream Decoding** (#11503): `decoderaddbytes`, `decoderbytesavailable`, `decoderconfigure`, `decoderempty`, `decodersetlineseps`, `decodertakeallchars`, `decodertakeavailablechars`, `decodertakebytes`, `decodertakechars`, `decodertakeline`
- **String** (#11495): `codepointfromname`, `codes`, `decodetocodes`, `encode`, `encodefromcodes`, `escape`, `fc`, `indexfrom`, `indexingoptimized`, `normalizecodes`, `ordfirst`, `ordbaseat`, `radix_I`, `replace`, `rindexfrom`, `sprintf`, `sprintfaddargumenthandler`, `sprintfdirectives`, `strfromname`, `substr_s`, `tc`, `tclc`, `unicmp_s`
- **System Introspection** (#11501): `backendconfig`, `cpucores`, `freemem`, `getenvhash`, `getsignals`, `totalmem`, `uname`, `UNAME_SYSNAME`, `UNAME_RELEASE`, `UNAME_VERSION`, `UNAME_MACHINE`
- **Threads** (#11502): `currentthread`, `newthread`, `threadid`, `threadjoin`, `threadlockcount`, `threadrun`, `threadyield`
- **Timish** (#11501): `decodelocaltime`, `sleep`
- **Trigonometric** (#11490): `acos_n`, `asin_n`, `atan_n`, `atan2_n`, `cos_n`, `cosh_n`, `sin_n`, `sinh_n`, `tan_n`, `tanh_n`
- **Type / Conversion** (#11492): `bool_I`, `bootarray`, `boothash`, `bootint`, `bootintarray`, `bootnum`, `bootnumarray`, `bootstr`, `bootstrarray`, `box_n`, `box_u`, `decont_i`, `decont_n`, `decont_s`, `fromI_I`, `fromnum_I`, `fromstr_I`, `isbig_I`, `iscoderef`, `iscont_i`, `iscont_n`, `iscont_s`, `ishash`, `isint`, `isinvokable`, `isnum`, `isprime_I`, `isrwcont`, `isstr`, `isttyfh`, `tonum_I`, `tostr_I`
- **Unicode Properties** (#11495): `getuniname`, `getuniprop_bool`, `hasuniprop`, `matchuniprop`, `unipvalcode`
- **Miscellaneous** (#11499): `getcodename`, `setdebugtypename`, `takeclosure`
- **Rakudo p6* (HLL)** (#11505): `p6argvmarray`, `p6bindassert`, `p6bindcaptosig`, `p6bindsig`, `p6box`, `p6capturelex`, `p6clearpre`, `p6decontrv`, `p6decontrv_6c`, `p6definite`, `p6getouterctx`, `p6invokeflat`, `p6isbindable`, `p6return`, `p6setautothreader`, `p6setfirstflag`, `p6setpre`, `p6sink`, `p6stateinit`, `p6staticouter`, `p6store`, `p6takefirstflag`, `p6trialbind`, `p6trybindsig`, `p6typecheckrv`

Out of scope (JS/JVM-only, `const` as a call, or rejected by Rakudo itself): `add_i64`, `sub_i64`, `atposref`, `push_o`, `shift_o`, `captureamedshash`, `coerce_sn`, `stringify`, `bindkey_o`, `falsey`, `iseq_snfg`, `isne_snfg`, `heap`, `instrumented`, `charsnfg`, `iscclassnfg`, `rindexfromend`, `substr2`, `substr3`, `substrnfg`, `RUSAGE_MSGRCVA`, `jvmclasspaths`, `jvmgetproperties`, `jvmgetunicodeversion`, `const`, `debugnoop`, `js`, `p6invokehandler`

## Not applicable

None recorded yet.
