# A closure call binds plain and sigilless positionals without the general binder

A sub called through a code variable (`my &g = &ga; g(self, $x)`), a callback
or a `my sub` captured into an anonymous method runs through the full closure
entry, `call_compiled_closure_in_unit`. Two light binds already served the
simplest signatures there, but both refused an instance or type-object
argument (the implicit `Any` check), any argument that came from a variable,
and every sigilless parameter. So the small helper subs real code passes
around -- AttrX::Mooish's `my sub get-attr-obj(Mu \obj, $attr) is raw`,
called as `get-attr-obj(self, $attr)` -- paid the whole general signature
binder, about 21k of a 38k-instruction call.

The full closure entry now has a third bind for those shapes
(`src/vm/vm_closure_entry_bind.rs`): plain positional `$` parameters
(optionally `is raw`) and sigilless `\x` / `Mu \x`, given plain values,
instances, type objects, `self`, or a variable that carries no type
constraint. It reproduces the one general-binder arm each argument reaches --
the item binding and readonly mark of a `$` parameter, the immutable value
binding and meta keys of a sigilless one, the implicit `Any` check, `@_` --
and declines everything else, such as a variable handed to a sigilless or
`is raw` parameter (which aliases its container), so the general binder still
runs and reports its own errors.

Callgrind slope per call (profiling build): `g(self, $_)` with
`(Mu \obj, $attr) is raw` 36,764 -> 25,000 Ir; `g($o, $_)` with
`($obj, $attr)` 27,537 -> 19,706. One AttrX::Mooish t/090 round
7,907,903 -> 7,799,499 Ir (-1.4%). This is stage 2 of #18267.
