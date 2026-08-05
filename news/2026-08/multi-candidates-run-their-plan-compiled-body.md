# A multi candidate runs the body its declaration plan compiled

ADR-0019 C6d-1 rewired every caller of the interpreter routine entry
`Interpreter::call_function_def` to run the routine's bytecode, but the entry itself survived
for one gated shape: a `state`-bearing candidate of a name declared with signature *alternates*,
`multi f(A $x) | (B $x) { state $c }`. Those alternates are ONE routine with ONE `state` cell,
which the compiler shares by threading a single `state_group` through every alternate's compiled
body — and that body never reached the candidate. `vm_register_sub_ops` walked the declaration
plan's `compiled_routine_keys` *after* registration and looked each one back up in the registry,
which works for a single sub (one name, one key) but not for a multi, where one name owns several
`FunctionDef`s under `/arity:types` keys with a `__m{N}` tiebreak. So it bailed out
(`if *multi { continue; }`), every multi candidate arrived with `compiled: None`, and an
on-the-fly compile per alternate — keyed on the per-alternate signature — handed each alternate
its own cell.

The fix is to stop rediscovering the owner and decide it where it is known. The plan already
records its compiled routines in declaration order (`compiled_routine_keys[0]` is the primary
signature, the rest follow `signature_alternates`), and registration installs the candidates in
that same order, so the VM now pairs them positionally and passes the routine *into*
registration. `register_sub_decl_with_metadata` fills `FunctionDef::compiled` from it — adapting
the bytecode to the signature registration itself derives (normalized `param_defs`, auto
`@_`/`%_`, empty-signature and rw/raw flags) — before the candidate is inserted. The
post-registration attach loop, and its `rsplit_once('/')` key derivation, are gone.

One more registration had to be covered. A sub declaration is registered twice: once from the
hoist pass at the top of its block, once in source order. Only the source-order site compiles the
body, so only it could record `compiled_routine_keys`. For a single sub that is invisible — the
later install replaces the bytecode-less hoisted one — but a multi candidate is *appended* to its
name's candidate set, so the hoisted candidate survived and was the one answering calls. The
source-order site now back-fills its keys into the hoisted plan for the same declaration (matched
by name and `sub_registration_fingerprint`).

That surfaced a latent collision worth its own regression test. A multi's compiled routine was
keyed by its *positional* signature alone, so two candidates differing only in their named
parameters — `multi f(Int :x($))` and `multi f(Int :y($))` — were both `Pkg::f/0`, and the second
body silently replaced the first in the compiled-function table. Dispatch tolerated that because
`vm_call_resolve` re-checks the body fingerprint before accepting a probe and otherwise falls back
to an on-the-fly compile. Installing bytecode by plan key does not tolerate it: it gave one
candidate the other's body, which made zef's `multi sub MAIN(Bool :version($) where .so)` print
the help screen. A colliding candidate now takes the fingerprinted key shape
(`Pkg::name/arity#fp`) that the same probe already tries next, so both candidates keep their own
bytecode.

Giving those candidates distinct keys then made resolution able to hand the *named*-argument
light-call path a per-candidate compiled body for the first time, which exposed a second latent
bug beside it: that path had no multi guard. Its cache is keyed by name alone, so a second call
with different named arguments reused the first call's candidate
(`multi earth(:$me!, :$him!)` answered with the `:$him!` candidate's body —
`roast/S06-multi/positional-vs-named.t`), and the light path pushes neither the multi-dispatch
frame nor the samewith context a candidate's `callsame` needs. It now carries the same
`!has_multi_candidates_cached(name)` guard the positional light path has always had, and for the
same stated reason. `t/multi-named-only-candidates.t` pins both halves, and raku agrees with
every case.

With the alternates sharing the plan's `state_group`-scoped body, the
`multi_candidate_state_forces_interpreter` gate, its `multi_alternate_signature_names` set, and
`Interpreter::call_function_def` are all deleted. `t/multi-signature-alternates.t` (alternates
share one cell) and `t/multi-candidate-state-otf.t` (ordinary multi candidates do not) are the two
poles, and both pass on bytecode alone. ADR-0019 C6d-1 now has a single item left: `exec_call`
still holds an inlined copy of the deleted entry's body, `run_block(&def.body)` included.
