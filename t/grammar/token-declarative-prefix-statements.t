use Test;

# A declaration's leading `:constant` / `:state` / `:my` statements run when the token is called
# through its `&name` (the first call included), whether or not the regex tree models them:
# the matcher keeps the text path that runs a declarative prefix.

plan 12;

{
    my token has-constant {
        :constant $x = 'foo';
        a $x
    }
    ok 'afoo' ~~ &has-constant, 'a :constant statement runs on the first call';
    is ~$/, 'afoo', '... and the match is the constant';
    nok 'abar' ~~ &has-constant, 'a different text does not match';
}

{
    my regex has-constant-regex { :constant $y = 'foo'; a $y }
    ok 'afoo' ~~ &has-constant-regex, 'the same in a regex declaration';
}

{
    my token has-state {
        :state $z++;
        c $z
    }
    ok 'c1' ~~ &has-state, 'a :state statement runs on the first call';
    is ~$/, 'c1', '... with the counter at 1';
    ok 'c2' ~~ &has-state, 'the next call sees the next value';
    is ~$/, 'c2', '... and matches it';
}

{
    my token has-my {
        :my $y = ' yack';
        b $y $y
    }
    ok 'b yack yack' ~~ &has-my, 'a :my statement runs on the first call';
    nok 'b yack shaving' ~~ &has-my, '... and a different text does not match';
}

{
    my $rx = rx/:constant $w = 'foo'; a $w/;
    ok 'afoo' ~~ $rx, 'a :constant statement in a regex literal';
    nok 'abar' ~~ $rx, '... and a different text does not match it';
}
