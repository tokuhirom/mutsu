# Three `Routine`/`Code` introspection gaps closed (#9818)

**`&foo.candidates».multi` answered `False`.** `routine_candidate_subs`
(`runtime/methods_signature_candidates.rs`) built each `.candidates` entry
without the `__mutsu_is_multi_candidate` env marker the `.multi` read arm
looks for, and even a correctly-marked candidate was shadowed: a
`ValueView::Sub`/`Routine`'s early `"is_dispatcher" | "multi"` handling in
`dispatch_routine_method`/`dispatch_sub_method` always answered the
whole-family case. `FunctionDef` carries no per-candidate `is_multi` flag of
its own, so the marker is now set from the candidate's own registry key
shape: a `multi sub`'s candidates are always keyed `pkg::name/<mangled-sig>`
(`registration_sub.rs`'s `multi_prefix`) even with only one candidate
declared so far, while a plain sub's single `pkg::name` key never is.

**`Food.^lookup("ingredients").line` threw `No such method 'line'`.**
`.^lookup`'s three Method-object builders had drifted: user-declared methods
and native/token methods both set a `line`/`file` attribute (`Nil` when
unknown), but the auto-generated attribute-accessor builder
(`wrap_accessor_method_object`) never inserted the keys at all, so dispatch
fell through to `X::Method::NotFound`. `CompiledAttrDecl` gained a
`decl_line` field, filled in by `Compiler::compile_class_attr_decls`
tracking the class body's own `Stmt::SetLine` markers (mirroring
`compile_method_body_keys`'s identical `decl_line` walk), and threaded
through `ClassAttributeDef::source_line`/`source_file` into the accessor's
`Method` object.

**`is implementation-detail` was a silent no-op.** The trait parsed and was
explicitly dropped — never recorded, never queued as a custom trait — so
`Code.is-implementation-detail` was not a real method at all; calling it on
a `Sub` fell into the ADR-0070 callable-compose fallback
(`&<composed-method:is-implementation-detail>`) and raised "No such method"
outright for a `Routine`-shaped builtin like `&say`. The trait now flows
through the existing `custom_traits` channel exactly like `is cached`, is
recorded on the registered `FunctionDef` as `is_implementation_detail`, and
`Code.is-implementation-detail` reads it back by a `(package, name)`
registry lookup — answering `False`, not an error, for anything with no
FunctionDef of its own.

Pinned by `t/routines/dispatch/routine-candidates-multi-flag.t`,
`t/lang/code-line-file-reflection.t`, and `t/oo/trait/is-implementation-detail-trait.t`.
