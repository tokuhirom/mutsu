# An enum declared in a sub no longer leaks its bare keys into the caller

`enum M2 <Normal X>` inside a sub stores its variant keys under the reserved
`__mutsu_enum_bare_` env prefix, but the call-return env merge only compared the
plain names recorded in `my_declared_enum_sym`, so the prefixed key was merged
back and replaced the caller's same-named key (`Normal.raku` answered
`M2::Normal` instead of `Volume::Normal`). `CompiledFunction::is_own_enum_key_sym`
now recognises the prefixed form and both return-merge paths use it (#11412).
