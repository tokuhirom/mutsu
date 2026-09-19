# Ecosystem multi dispatch and dependent role defaults

Exported proto families now refresh their imported multi candidates when later
members are registered. Candidate `where` and default expressions also run in
the candidate package, and qualified proto redispatch counts only positional
arguments. Together these fixes let `Math::Fitting` resolve its exported
predicates and named `Fit` candidates.

Hyper calls now recognize instances that provide `CALL-ME`, so a callable model
returned by `Math::Fitting` can be applied with `>>.&callable`.

Role attribute defaults see attributes seeded earlier in the same role
composition, and runtime role composition supplies the correct `self` while
evaluating those defaults. This covers the dependent `base-name`/`builder`
attributes used by `AttrX::Lazy`.

Regression coverage is in `t/modules/import-export/exported-proto-multi-family.t`,
`t/routines/hyper-callable-instance.t`, and
`t/oo/role/dependent-role-attribute-default.t`.

The remaining multidimensional `Math::Fitting` files are blocked by the
runtime `use Math::Matrix` / `AttrX::Lazy` interaction recorded in mutsu issue
#8806.
