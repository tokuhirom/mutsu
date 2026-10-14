# A `{*}`-only proto no longer binds its own signature

Rakudo's dispatcher goes straight to the candidates for a proto whose body is just `{*}`;
mutsu type-checked the arguments against the proto's signature first, so a call that only
a candidate accepts died with a binding error (CSS::Writer `write-num`, #13250). Both
`proto sub` and `proto method` now skip the proto's own binding.
