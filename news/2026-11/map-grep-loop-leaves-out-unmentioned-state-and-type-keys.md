# The map/grep inline loop leaves out the state and lexical-type keys its body never names

With a module loaded, a `.map`/`.grep` callback's captured env held 13 names the
inline loop installed in the consuming frame and removed again for every chunk:
three lexical-type keys (`\u{1}Stream`, ...) and six `__mutsu_state_key::<name>`
metadata keys beside `?FILE`, `=pod`, `!` and `__mutsu_callable_type`.

`droppable_subject` now also covers the two key shapes whose only readers name
something the body would have to mention: a lexical type's own key
(`\u{1}Foo`, consulted by a reader that names the type `Foo`) and a `state`
variable's metadata key (`__mutsu_state_key::<name>`, read only for a free
variable `<name>` of the running body). A body that does mention the type or the
variable keeps them; `t/routines/closure/map-grep-closure-state-and-lexical-type-captures.t`
pins both directions and agrees with `raku`.
