use Test;

plan 15;

# A Method object from .^lookup / .^find_method is a Routine in real
# Rakudo, so it answers .arity / .count like any other -- both are
# derivable from the .signature it already carries, which always
# prepends the invocant (#8416).

{
    class B1 { method foo($a) {} }
    my $m = B1.^lookup("foo");
    is $m.arity, 2, 'the invocant plus one required positional -> arity 2';
    is $m.count, 2, 'and count 2';
}

{
    class B2 { method foo($a, $b, $c?) {} }
    my $m = B2.^lookup("foo");
    is $m.arity, 3, 'invocant + two required -> arity counts only required';
    is $m.count, 4, 'count counts required and optional too';
}

{
    class B3 { method foo($a, *@rest) {} }
    my $m = B3.^lookup("foo");
    is $m.arity, 2, 'a slurpy is not required';
    is $m.count, Inf, 'but makes count infinite';
}

{
    class B4 { method foo() {} }
    my $m = B4.^lookup("foo");
    is $m.arity, 1, 'no declared params -> arity is just the invocant';
    is $m.count, 1, 'and count 1';
}

# The same gap, on the sibling Instance shape a grammar token/rule/regex
# answers from .^lookup (a Regex, not a Method -- but built by the same
# code and sharing the same gap).

{
    grammar G1 { token foo ($x) { \d+ } }
    my $t = G1.^lookup("foo");
    is $t.arity, 2, 'a token declaring one param -> arity 2 (invocant + $x)';
    is $t.count, 2, 'and count 2';
    is $t.signature.raku, ':(G1 $:: $x, *%_)',
        'the token threads its real declared param into .signature too';
}

{
    grammar G2 { token foo { \d+ } }
    my $t = G2.^lookup("foo");
    is $t.arity, 1, 'a token with no declared params -> arity is just the invocant';
    is $t.count, 1, 'and count 1';
    is $t.signature.raku, ':(G2 $:: *%_)',
        'and .signature carries just the invocant + *%_';
}

# What must keep working: an ordinary native (Rust-implemented) method's
# generic synthesized signature is untouched by this.
{
    my $native = Str.^lookup("uc");
    lives-ok { $native.signature }, 'a native method signature read still lives';
}
