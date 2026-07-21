use Test;

plan 8;

# A user-declared `infix:<->` (which registers `-` as a user infix symbol) must
# NOT make the parser treat the `-` in a `->` pointy-block arrow as subtraction.
# Regression: `multi infix:<->(...)` earlier broke a later `for @a -> $x { }` in
# the same lexical scope with "expected expression after infix operator".
{
    multi infix:<->(Str:D $a, Str:D $b) is export { $a ~ $b }

    # The custom infix still works...
    is ("ab" - "cd"), 'abcd', 'user infix:<-> applies to declared types';
    # ...and the built-in Int subtraction is unaffected.
    is (10 - 3), 7, 'built-in Int subtraction still works alongside a user infix:<->';

    # A pointy block after the user infix:<-> parses as a lambda, not `- >`.
    my @out;
    for 1, 2, 3 -> $x { @out.push: $x * 2 }
    is @out, [2, 4, 6], 'for ... -> $x block parses after a user infix:<->';

    my $sq = -> $n { $n * $n };
    is $sq(5), 25, 'bare pointy lambda parses after a user infix:<->';

    # Return arrow `-->` still works too.
    my sub twice($n --> Int) { $n * 2 }
    is twice(6), 12, 'return arrow --> parses after a user infix:<->';
}

# Auto-decrement / compound-assign are not swallowed by a user infix:<->.
{
    multi infix:<->(Str:D $a, Str:D $b) is export { $a ~ $b }
    my $i = 5;
    $i--;
    is $i, 4, 'post-decrement -- parses after a user infix:<->';
    my $j = 10;
    $j -= 3;
    is $j, 7, 'compound assignment -= parses after a user infix:<->';
}

# A method body with a pointy loop after operator overloads (the IP::Addr shape).
{
    my class C {
        multi infix:<->(C:D $a, Int:D $b) is export { $a }
        method items(--> List) {
            my @list;
            for 1, 2, 3 -> $ip { @list.push: $ip }
            @list;
        }
    }
    is C.items, [1, 2, 3], 'class method pointy loop parses after infix:<-> overload';
}
