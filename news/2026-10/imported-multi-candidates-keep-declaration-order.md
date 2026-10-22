# Imported multi candidates keep declaration order

When a block-scoped `use` imports a multi family, the candidates were captured in the
registry's key-walk (hash) order, and equally narrow `where` candidates were then ranked
by that order instead of by declaration. `factorial(0)` from `Math::SpecialFunctions`
(`where $n <= 1` declared before `where $n %% 2`) therefore answered 0, not 1. The
captured candidates are now ordered by declaration, so imported families break ties the
way the declaring module does. All four test files of `Math::SpecialFunctions` now match
rakudo.
