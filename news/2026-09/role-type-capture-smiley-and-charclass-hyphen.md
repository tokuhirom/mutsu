# Role type captures with `:U`/`:D` across modules, and `<:Prop-[...]>` character classes

Found by an ecosystem roulette draw of MUGS::UI::CLI, whose six game-UI modules
all died at load time.

**`T:U` inside a composed parametric role.** A role such as
`role Registry[::Constraint] { method register(Constraint:U $class) { ... } }`
composed as `does Registry[Game]` resolves `Constraint` at call time from the
role bindings stored at composition. Composition stored the binding
(`Constraint => Game`) but not the type-capture marker. A bare `Constraint $x`
still resolved through the env value. The smiley forms (`Constraint:U`,
`Constraint:D`) resolve only through that marker, so they kept the literal
constraint name and rejected every argument with
`expected Constraint:U but got Game`. The marker only reached a method's env when
it leaked out of the declaring compunit, which happened when the program `use`d
that module directly but not when it was loaded transitively (and not at all
when the role body has a `my` variable). Composition now records the marker next
to each type-object binding. MUGS::UI::CLI's `MUGS::UI.register-ui(..., self.WHAT)`
now binds, and all six game UIs load.

**`-` directly after a Unicode property.** In `<:Cs-[\n]>` the `-` is the
difference operator, but the parse-time character-class validator treated it as
part of the property name. It then reported "Missing + or - in character class
expression" for `/<:C+:Cc+:Cf+:Cn+:Co+:Cs-[\n]>+/` (MUGS::App::CLI). A hyphen now
belongs to the name only when a letter follows, the same rule that applies to
identifiers.

Tests: `t/oo/role/role-type-capture-smiley-transitive-load.t`,
`t/regex/syntax/charclass-unicode-property-hyphen-operator.t`.
