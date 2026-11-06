# A bare file's exported operator multis are not a proto

Importing a bare (non-`unit`) module file whose `is export` multis declare a
core operator (Color's `multi infix:<*>(Color, Real)`) recorded the operator
name in `proto_subs`, as if the module had declared a `proto`. A `proto`
shadows the core routine, so a user `multi infix:<*>(Unit $v, Numeric $n)` in
the importing scope stopped competing with the core candidates and won every
call, including its own `($v * $n)` body, which recursed until "Too deep
recursion". That one gap sat behind nearly every test file of CSS::Properties
(`CSS::Units` plus `use Color`), and the same signature in CSS::Font::Resources,
CSS::Stylesheet and CSS::TagSet. Only a real `proto` (or a non-operator name,
unchanged) now marks the imported name as a proto.
