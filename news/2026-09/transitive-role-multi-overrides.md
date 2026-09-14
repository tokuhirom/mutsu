`role Child does Parent` now lets a same-signature `multi method` in `Child`
replace the inherited candidate from `Parent` when `Child` is composed into a
class. The inherited candidate is removed before dispatch, while an ancestor
that the class composes explicitly remains an independent conflict. This fixes
the `Red::Driver::Pg` load failure found in [#7988](https://github.com/tokuhirom/mutsu/issues/7988).
