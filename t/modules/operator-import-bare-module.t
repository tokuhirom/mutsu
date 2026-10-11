use lib 't/lib';
use Test;

# Distilled from MUGS::UI::TUI (Terminal::Widgets::ColorThemes): a bare module
# file imports an operator and calls it after its own load has returned, and a
# `unit module` that `use`s such a file must not capture the bare file's import.

plan 3;

use OpBare::Bare;
is bare-op('d'), 'c/d', 'operator imported by a bare module file works in its exported sub';

use OpBare::Unit;
is $BARE-CONST, 'a/b', 'a bare file loaded from a unit module sees its imported operator';

throws-like { EVAL 'my $x = "a" ¢¿ "b"' }, X::Syntax::Confused,
    'the operator is not leaked into the importing scope';
