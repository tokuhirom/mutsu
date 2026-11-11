# A start rule named with its candidate keeps its `:my $*x` frame

`.subparse(:rule('routine-declarator:sym<pattern>'))` looked up the start rule's
`:my $*x` declarations under the full candidate name, while they are recorded
under the bare rule name (`routine-declarator`). No dynamic-variable frame was
set up, so the start rule's action saw the value the `:my` declared, not what
an action of an inner production assigned since. The slang host parses each
slang production this way: EEL 0.3's `pattern` declarator read an undefined
`$*EEL-DEFINITION` (and so declared a `proto`) whenever the pattern's body held
a parenthesized argument (#16888).
