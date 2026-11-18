# Punning a role no longer re-declares its exported sub in the caller's package

A role body's `sub ... is export` reads the body's lexicals, such as a
`my constant %Dimension`. When a role was punned (`CSS::Units.value(...)`
calling a method on the role itself), `run_role_body_for_composition` ran
the body's statements again and let an exported plain `sub` register in the
*current* package, which is whatever module's code triggered the pun. That
copy shadowed the module's own import of the sub, and it looked the role
body's lexicals up under the module's package, where they are not, so it
answered `Any`.

The composition twin, `run_composed_role_deferred_body_inner`, already ran
an exported plain sub in the role's own package. The pun path now does the
same; only a `multi sub` still registers where the composer can reach it.

CSS::Properties' `t/calc.t` and `t/measure.t`, whose `CSS::Properties::Calculator`
scaled `px` as 1 ("Unknown units: px") after a `calc(...)`, now match
rakudo (#18106).
