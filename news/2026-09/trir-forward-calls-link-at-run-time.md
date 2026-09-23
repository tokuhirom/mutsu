# A TRIR routine's call to a later-declared routine is 50x cheaper

ADR-0111 Step 1. A TRIR body statically links a call (`CallTr`) only to a
routine declared *before* it, because that is all its compile has seen. Every
forward call was a `CallGen`, and paid for it:

- its arguments were boxed and containerized;
- the callee was resolved by name through `call_function`'s whole chain;
- the callee then ran on the *untyped* path, even though it had a chunk of
  its own.

Measured at **8,857 ns a call**, against 172 ns for a linked call, 1,896 ns
for the same call with TRIR off, and 80 ns in rakudo. JSON::Fast pays it on
every value, because `parse-obj` and `parse-array` call `parse-thing`,
declared after them. Mutual recursion makes one direction a forward reference
whatever the order.

The site is now linked at run time, to what the generic dispatch actually did
(`src/trir/gen_link.rs`):

- The first call goes the generic way, with an observer armed for the callee
  name.
- `call_function_fallback`'s plain user-routine branch records the def it
  picks, but not when multi dispatch is involved.
- If that def has a chunk, the site keeps it, keyed by the three inputs the
  resolution read: `fn_resolve_gen`, the current package, and the running
  frame's lexical package.
- Later calls in the same state bind their arguments by `CallTr`'s own
  checks, peeking before consuming anything, and run the chunk.
- A wrapped callee, a junction or aggregate argument, an unbindable type, or a
  changed state takes the generic path instead.

The one subtle part is that observers nest. The untyped callee of a generic
call can reach a TRIR routine that makes a generic call of its own. The first
version cleared the observer there, and lost exactly the link that mattered:
`parse-thing` reached from `parse-array`, whose body calls
`parse-string-slow`. The enclosing observer is now set aside and restored.

The ADR had proposed linking at compile time, when the enclosing scope
finishes. That needed three things:

- deferring every routine's TRIR compile with its AST retained;
- re-attaching chunks into nested-sub tables that have already moved;
- recompiling callers when a callee declines.

The run-time link answers the same question with the routine the program
actually dispatched to. It also covers calls into other compunits. The ADR's
implementation status records the change of mechanism.

Measured on a release build in a 4-core container:

| | before | after |
|---|---:|---:|
| forward-call microbenchmark | 8,857 ns | 174.5 ns (the same as a backward `CallTr`) |
| TRIR entries from outside, 100-record decode | 1,949 | 70 |
| calls served through a link, same run | — | 1,088 |
| 727-record `from-json` | 1.67 s | ~1.55 s |

The wall-clock gain is the ~0.07 s ADR-0111 §3 estimated for this step. About
80% of the decode is still the slow string path (`parse-string-slow` /
`unjsonify-string`, which are not TRIR at all), and that is Step 2. The decoded
result is byte-identical to rakudo's.

Pinned by `t/vm/codegen/adr0111-trir-forward-link.t`, which covers:

- a forward `is rw` write;
- mutual recursion;
- an aggregate argument declining, followed by a scalar one linking;
- a `.wrap` installed after the site linked.

Every case requires TRIR on and off to agree, and `gen-links` to be non-zero.
`MUTSU_VM_STATS`' `trir:` line gains the `gen-links=` count.
