# `refine_slang` reads the members of a lexical or anonymous role

`$*LANG.refine_slang('MAIN', role { }, role { method numish(Mu $/) { ... } })`
silently ignored the anonymous actions role (it was taken as empty), and a
lexical `my role` was looked up by a site-key name that the role registry does not
use. `slang_role_rule_names` now normalises the role value to its group key like
every other consumer of a role type object, so both shapes register their tokens and
action methods; the empty-anonymous-role special case is gone. (#17008)
