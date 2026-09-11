The sigil-prefixed `$<name>=<.subrule>` grammar capture form now preserves the
subrule action name on its visible alias. Actions therefore populate `.ast` and
`.made` just as they do for the equivalent `<name=.subrule>` spelling.

The regression is covered by `t/grammar/grammar-sigilless-alias-silent-action.t`.

Fixes #7910.
