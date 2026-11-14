# A slang's own `trait_mod` candidate runs where traits are parsed

`use OO::Plugin` now loads, as it does on rakudo 2026.09. Its slang adds four `trait_mod`
candidates (`for`, `after`, `before`, `demand`), and slang activation used to refuse them with
"grammar rule override 'trait_mod:sym<for>' is not supported", so every file that loaded the module
died at the `use`.

A `trait_mod:sym<...>` candidate (rakudo's RakuAST grammar spells this category with an
underscore) is now a production hook, like the other ADR-15762 categories. It is tried first
wherever mutsu parses a declaration's traits: classes, roles, packages, grammars and routines.
OO::Plugin's candidates guard themselves with `<.in-plug-class>` / `<.in-plugin>`, which panic
outside the module's own declarators. As on rakudo, `class D for Int { }` is therefore the compile
error "Modificator 'for' is only applicable to a plug-class", and every ordinary trait is untouched.

A candidate written as a plain leading group (`[ <sym> {} <.check> ] ...`) is keyed on its keyword,
so the host is asked only where that keyword stands. A unit with no slang trait pays one check per
trait position.

Applying a trait node such a candidate's action attaches is not supported yet. It is a loud
compile error, not a silent ignore.
