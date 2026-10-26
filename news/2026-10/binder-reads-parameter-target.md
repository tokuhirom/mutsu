# The binder and dispatch read a parameter's target form

About two dozen checks in the runtime, VM and dispatcher that compared a parameter's name with
`__type_only__`, `__subsig__` or the `__type_capture__` prefix now ask `ParamDef::is_type_only`,
`is_parens` or `is_bare_type_capture`. The placeholder names remain as binding slots. ADR-10723
section 6.116.
