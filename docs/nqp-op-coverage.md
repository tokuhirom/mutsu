# `nqp::` op coverage

Which of the `nqp::` ops a Raku program can call does mutsu implement? This is
the inventory the `nqp::` coverage campaign
([#11488](https://github.com/tokuhirom/mutsu/issues/11488)) is tracked against.
The campaign replaces the 2026-07 "add `nqp::` ops on demand only" rule
(`news/2026-07/nqp-op-layer-measured-and-rejected.md`): per distribution the
op set is a threshold function, so the whole documented set is closed category
by category instead of one op per failing dist.

**Measured 2026-10-04** with

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
| Arithmetic | 26 / 26 | 0 | #11490 |
| Array | 60 / 60 | 0 | #11493 |
| Asynchronous | 0 / 11 | 11 | #11502 |
| Atomic | 11 / 11 | 0 | #11502 |
| Binary Data | 6 / 6 | 0 |  |
| Bit | 15 / 15 | 0 | #11491 |
| Captures | 5 / 5 | 0 | #11496 |
| Coercion | 10 / 10 | 0 | #11553 |
| Conditional | 5 / 5 | 0 | #11500 |
| Context Introspection | 4 / 24 | 20 | #11498 |
| Loop/Control | 6 / 6 | 0 | #11500 |
| Exception Handling | 15 / 15 | 0 | #11497 |
| Processes | 4 / 4 | 0 | #11501 |
| File / Directory / Network | 25 / 25 | 0 | #11501 |
| Hash | 8 / 8 | 0 | #11494 |
| HLL-Specific | 3 / 13 | 10 | #11504 |
| Input/Output | 14 / 14 | 0 | #11501 |
| Relational / Logic | 40 / 40 | 0 | #11491 |
| NativeCall | 6 / 7 | 1 | #11504 |
| Numeric | 17 / 17 | 0 | #11490 |
| Objects | 29 / 31 | 2 | #11499 |
| Parametric Extensions | 0 / 5 | 5 | #11499 |
| Profiling | 0 / 3 | 3 | #11504 |
| Serialization context | 1 / 18 | 17 | #11504 |
| Stream Decoding | 10 / 10 | 0 | #11503 |
| String | 48 / 48 | 0 | #11495 |
| System Introspection | 29 / 29 | 0 | #11501 |
| Threads | 7 / 7 | 0 | #11502 |
| Timish | 3 / 3 | 0 | #11501 |
| Trigonometric | 10 / 10 | 0 | #11490 |
| Type / Conversion | 36 / 53 | 17 | #11553 |
| Unicode Properties | 8 / 8 | 0 | #11495 |
| Miscellaneous | 4 / 4 | 0 | #11499 |
| Rakudo p6* (HLL) | 17 / 26 | 9 | #11505 |
| **Total** | **482 / 577** | **95** | |

## Missing ops by category

- **Asynchronous** (#11502): `asyncconnect`, `asynclisten`, `asyncreadbytes`, `asyncwritebytes`, `cancel`, `killprocasync`, `permit`, `signal`, `spawnprocasync`, `timer`, `watchfile`
- **Context Introspection** (#11498): `bindlex`, `bindlex_i`, `bindlex_n`, `bindlex_s`, `bindlexdyn`, `ctxouter`, `curlexpad`, `getlex`, `getlex_i`, `getlex_n`, `getlex_s`, `getlexcaller`, `getlexouter`, `getlexref_i`, `getlexref_n`, `getlexref_s`, `getlexrel`, `getlexrelcaller`, `getlexreldyn`, `lexprimspec`
- **HLL-Specific** (#11504): `bindcurhllsym`, `getcurhllsym`, `hllboxtype_i`, `hllboxtype_n`, `hllboxtype_s`, `hllhash`, `hlllist`, `sethllconfig`, `usecompileehllconfig`, `usecompilerhllconfig`
- **NativeCall** (#11504): `nativecallinvoke`
- **Objects** (#11499): `rebless`, `setwho`
- **Parametric Extensions** (#11499): `setparameterizer`, `parameterizetype`, `typeparameterat`, `typeparameterized`, `typeparameters`
- **Profiling** (#11504): `force_gc`, `mvmendprofile`, `mvmstartprofile`
- **Serialization context** (#11504): `createsc`, `deserialize`, `forceouterctx`, `freshcoderef`, `getobjsc`, `markcodestatic`, `popcompsc`, `pushcompsc`, `scgetdesc`, `scgethandle`, `scgetobjidx`, `scobjcount`, `scsetcode`, `scsetdesc`, `scsetobj`, `serialize`, `setobjsc`
- **Type / Conversion** (#11553): `bootarray`, `boothash`, `bootint`, `bootintarray`, `bootnum`, `bootnumarray`, `bootstr`, `bootstrarray`, `iscoderef`, `iscont_i`, `iscont_n`, `iscont_s`, `ishash`, `isint`, `isnum`, `isrwcont`, `isstr`
- **Rakudo p6* (HLL)** (#11505): `p6argvmarray`, `p6bindsig`, `p6clearpre`, `p6setfirstflag`, `p6setpre`, `p6stateinit`, `p6staticouter`, `p6takefirstflag`, `p6trybindsig`

Out of scope (JS/JVM-only, `const` as a call, or rejected by Rakudo itself): `add_i64`, `sub_i64`, `atposref`, `push_o`, `shift_o`, `captureamedshash`, `coerce_sn`, `stringify`, `bindkey_o`, `falsey`, `iseq_snfg`, `isne_snfg`, `heap`, `instrumented`, `charsnfg`, `iscclassnfg`, `rindexfromend`, `substr2`, `substr3`, `substrnfg`, `RUSAGE_MSGRCVA`, `jvmclasspaths`, `jvmgetproperties`, `jvmgetunicodeversion`, `const`, `debugnoop`, `js`, `p6invokehandler`

## Not applicable

- `atkey_i`: Rakudo dies on every reachable REPR: VMHash "does not support native type storage", CStruct "does not support associative access".
- `atkey_n`: Rakudo dies on every reachable REPR: VMHash "does not support native type storage", CStruct "does not support associative access".
- `atkey_s`: Rakudo dies on every reachable REPR: VMHash "does not support native type storage", CStruct "does not support associative access".
- `atkey_u`: Rakudo dies on every reachable REPR: VMHash "does not support native type storage", CStruct "does not support associative access".
- `bindkey_i`: Rakudo dies on every reachable REPR: VMHash "does not support native type storage", CStruct "does not support associative access".
- `bindkey_n`: Rakudo dies on every reachable REPR: VMHash "does not support native type storage", CStruct "does not support associative access".
- `bindkey_s`: Rakudo dies on every reachable REPR: VMHash "does not support native type storage", CStruct "does not support associative access".
- `for`: Rakudo rejects every Raku call at compile time ("The 'for' op expects a block as its second operand, got QAST::Op"): a Raku block literal compiles to a closure op, never the bare QAST::Block the op requires (NQP's own `nqp::for` fails the same way).
- `list_b`: Rakudo rejects every Raku call at compile time ("The 'list_b' op needs a list of blocks, got QAST::Op"): a Raku block literal never compiles to the bare QAST::Block the op requires.
