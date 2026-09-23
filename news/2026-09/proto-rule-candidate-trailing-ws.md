# A `proto rule` candidate no longer swallows trailing whitespace

A `rule` ends in an implicit `<.ws>` only when its body has whitespace before
the closing `}` — `rule r {'a' <x> ')'}` stops right after the `)`, while
`rule r {'a' <x> ')' }` also eats the whitespace that follows. mutsu got this
right for ordinary rules, but the named-declarator path in
`parser/stmt/class/grammar_module.rs` appended an unconditional `<.ws>?` to
every `:sym<...>` candidate of a `proto rule`. So a candidate written with no
trailing space still consumed the input whitespace after it.

The witness was `CSS::Module::CSS3::Selectors`' `t/00basic.t`: on
`li:not(.pingback) .comment-content`, CSS::Grammar::CSS21's
`rule pseudo-function:sym<negation> {:i'not(' ... ')'}` swallowed the space
after `)`, so `.comment-content` was absorbed into the first simple selector
instead of starting a second one.

The special case is gone; a candidate now takes exactly the implicit
whitespace any other `rule` with the same body would, which matches Rakudo.
Pinned by `t/grammar/grammar-proto-rule-candidate-trailing-ws.t`
(mutsu#9094).
