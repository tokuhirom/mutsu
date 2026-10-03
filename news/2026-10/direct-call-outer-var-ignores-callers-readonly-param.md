# A sub called by name can assign an outer variable named like its caller's parameter

`my $x = 42; sub g4 { $x = 7 }; sub c6($x) { g4() }` died in `g4` with "Cannot assign to a
readonly variable or a value" (#11165). The write targets the mainline `my $x`, which is
writable; the refusal came from `c6`'s readonly parameter, which the name-keyed readonly
registry still held while `g4` ran. #11134 had fixed only the code-value call (`my $s = &g4;
$s()`).

Following ADR-11142 §2.3, the binding now answers. The shared cell a `my` declaration gets
when a nested routine writes it by name records that its declaring frame decided it is
writable (`ContainerCell::set_binding_decision`), and the declaration-time readonly marks
(`constant`, the `is List`/`Map` traits, immutable `:=` binds, sigilless terms) re-decide it as
readonly. `CheckReadOnly`, the by-name `SetGlobal` store and the named `++`/`--`/`OP=` check
resolve a free variable's binding first: a readonly kind refuses, a writable decision skips the
registry, and only an undecided binding still asks it. A parameter or loop alias captured by a
nested routine stays undecided, so those writes are still refused as before.

Found alongside: #11263 (a `constant` anywhere in the file hides an immutable `:=` binding's
kind from other frames).
