use v6;
use Test;

# The colon method-call form `$obj.method:` takes the rest of the statement as
# its argument list. When the colon sits immediately before a block-closing `}`
# (an empty argument list at the end of a block), Raku treats it as a plain
# zero-argument `.method()` call. mutsu previously tried to parse a first
# argument and failed with "right-hand expression after '='" — the blocker
# behind CSV::Table's `$!rowname-width = $row.rwid:` followed by a closing `}`.
#
# NOTE: Raku accepts the empty colon form ONLY before `}`; before `;`, `)`, `]`
# or end of input it still demands a colon-pair. This pin matches that.

plan 6;

class C {
    has $.v = 5;
    method twice($n = 1) { $!v * $n }
}

my $r = C.new;

# Empty colon-call before a closing brace (the CSV::Table shape).
my $w;
if 1 {
    $w = $r.v:
}
is $w, 5, 'empty colon-call before } is a no-arg call';

# Empty colon-call before `}` on the same line.
my $w2;
if 1 { $w2 = $r.v: }
is $w2, 5, 'empty colon-call before } on one line is a no-arg call';

# Empty colon-call on a method with an optional param uses the default.
my $y;
if 1 {
    $y = $r.twice:
}
is $y, 5, 'empty colon-call uses the parameter default';

# A bare (non-assignment) empty colon-call statement before `}`.
my $seen;
if 1 {
    $seen = $r.twice:
}
is $seen, 5, 'bare empty colon-call before } works';

# The non-empty colon-arg forms must still work (parenthesised so the colon
# form does not swallow the test description as an argument).
is ($r.twice: 3), 15, 'colon-call with a positional arg still works';

my @list = <a b c>;
is (@list.join: '-'), 'a-b-c', 'colon-call with a string arg still works';

done-testing;
