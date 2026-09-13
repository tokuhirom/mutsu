use v6;
use Test;

# A statement takes at most one conditional modifier and then at most one loop
# modifier. A further modifier is not that statement's business — it belongs to
# whatever encloses it, and `do STMT` is the construct that can be that
# enclosure:
#
#     do return False unless %h<auth> ~~ $!auth if $!auth;
#
# (Pakku::Spec, ten more Pakku compunits behind it) is a `do`-wrapped `return …
# unless …`, and the `if …` modifies the `do` statement itself. mutsu raised
# "Missing semicolon" the moment it saw the second keyword, so it rejected this.
# The diagnosis now waits until the statement list finds the keyword unconsumed,
# which is where rakudo raises it too.

plan 9;

{
    sub f($x) {
        do return 'early' unless $x ~~ 1 if $x;
        'late'
    }
    is f(2), 'early', 'outer `if` true and inner `unless` true: the `do return` fires';
    is f(1), 'late',  'outer `if` true, inner `unless` false: falls through';
    is f(0), 'late',  'outer `if` false: the whole `do` statement is skipped';
}

# The `do`-wrapped statement may carry a loop modifier of its own too.
{
    my @seen;
    my $on = 1;
    do @seen.push: $_ for 1, 2 if $on;
    is @seen.join(','), '1,2', 'do STMT for LIST if COND runs the loop';
    @seen = ();
    $on = 0;
    do @seen.push: $_ for 1, 2 if $on;
    is @seen.elems, 0, 'and the outer `if` still gates it';
}

# Without the `do` there is no enclosing statement to take the second keyword,
# so the chain is still X::Syntax::Confused ("Missing semicolon") — and now it
# carries a source location, as rakudo's does.
throws-like 'say 1 if 2 if 3', X::Syntax::Confused,
    'two conditionals are still rejected';
throws-like 'say 1 for 1..2 for 3..4', X::Syntax::Confused,
    'two loops are still rejected';
throws-like 'say 1 for 3..4 if 2', X::Syntax::Confused,
    'a loop then a conditional is still rejected';
throws-like 'sub f($x) { do return 1 unless $x if $x if $x }', X::Syntax::Confused,
    'a `do` absorbs one extra modifier, not two';
