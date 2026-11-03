# `<.:Lu>`, `<.[b]>` and `<.+:N>` are character classes

A leading `.` before a character-class form (`<.:So>`, `<.[b]>`, `<.+:N>`,
`<.-[x]>`) is the implicit positive half of the class: rakudo builds the same
tree as without the dot. The regex tree parser accepted the dot only before `+`
and `-`, so a unit with `<.:So>` (Slang::Emoji's `token sigilless-variable`)
failed to round-trip through RakuAST, and the runtime pattern parser read
`<.[b]>` as a non-capturing subrule call that matched the wrong text. Both now
treat the dotted form as the class itself.
