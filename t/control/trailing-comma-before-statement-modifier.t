use v6;
use Test;

# A trailing comma in a statement's argument list is an empty list slot, not a
# syntax error: `die "x", if @c;` is legal Raku. mutsu used to fail to parse it —
# the argument-list parsers only treated `;`, `}`, `)` and end-of-input as
# terminators after a comma, so they tried to read the modifier keyword as a term.
#
# Found in `UpRooted::Table`, which writes
#     die sprintf '...', $a, $b,
#         if %!columns.values.grep: *.order == $column.order;

plan 12;

# `die` / `fail` — a single argument expression (no comma parser involved).
lives-ok { EVAL 'die "x", if 0' }, 'die with a trailing comma before `if`';
lives-ok { EVAL 'die "x", unless 1' }, 'die with a trailing comma before `unless`';
lives-ok { EVAL 'die "x", for ()' }, 'die with a trailing comma before `for`';

# `return` — goes through the comma-list parser.
{
    sub skips { return 1, if 0; 2 }
    is skips(), 2, 'return with a trailing comma before a false `if` falls through';
    sub takes { return 5, if 1; 2 }
    is takes(), 5, 'and returns its value when the modifier is true';
    sub listy { return 1, 2, if 1 }
    is-deeply listy(), (1, 2), 'a multi-element return keeps its list';
}

# Paren-less listop argument lists, both the builtin head and the general one.
{
    my @c;
    lives-ok { EVAL 'my @c; die sprintf "a %s", 1, if @c' },
        'a builtin listop head (sprintf) with a trailing comma before `if`';
    lives-ok { EVAL 'my @c; say join ",", 1, if @c' },
        'another builtin listop head (join)';
    is (EVAL 'my $x = sprintf "%s", 1, if 0; $x // "unset"'), 'unset',
        'the modifier still governs the whole statement';
}

# A same-named PAIR KEY must not be mistaken for a statement modifier, or the
# comma list would be truncated at its last element.
{
    my @a = 1, with => 2;
    is-deeply @a, [1, (with => 2)], 'a `with =>` pair key still parses as a pair';
    my @b = 1, if => 2;
    is-deeply @b, [1, (if => 2)], 'an `if =>` pair key too';
}

# Plain trailing commas keep working.
{
    my @a = 1, 2,;
    is @a.elems, 2, 'an ordinary trailing comma still yields a 2-element list';
}
