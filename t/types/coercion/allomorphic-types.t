use Test;

plan 41;

# IntStr from angle brackets
{
    my $x = <42>;
    is $x.WHAT.gist, '(IntStr)', '<42>.WHAT is IntStr';
    ok $x ~~ Int, '<42> ~~ Int';
    ok $x ~~ Str, '<42> ~~ Str';
    ok $x ~~ IntStr, '<42> ~~ IntStr';
    ok $x ~~ Allomorph, '<42> ~~ Allomorph';
    is +$x, 42, '+<42> is 42';
    is ~$x, '42', '~<42> is "42"';
}

# RatStr from angle brackets (decimal)
{
    my $x = <3.14>;
    is $x.WHAT.gist, '(RatStr)', '<3.14>.WHAT is RatStr';
    ok $x ~~ Rat, '<3.14> ~~ Rat';
    ok $x ~~ Str, '<3.14> ~~ Str';
    ok $x ~~ RatStr, '<3.14> ~~ RatStr';
    ok $x ~~ Allomorph, '<3.14> ~~ Allomorph';
    is +$x, 3.14, '+<3.14> is 3.14';
    is ~$x, '3.14', '~<3.14> is "3.14"';
}

# NumStr from angle brackets (scientific notation)
{
    my $x = <1e2>;
    is $x.WHAT.gist, '(NumStr)', '<1e2>.WHAT is NumStr';
    ok $x ~~ Num, '<1e2> ~~ Num';
    ok $x ~~ Str, '<1e2> ~~ Str';
    ok $x ~~ NumStr, '<1e2> ~~ NumStr';
    ok $x ~~ Allomorph, '<1e2> ~~ Allomorph';
    is +$x, 100, '+<1e2> is 100';
}

# IntStr.new constructor
{
    my $x = IntStr.new(42, "forty-two");
    is $x.WHAT.gist, '(IntStr)', 'IntStr.new.WHAT is IntStr';
    ok $x ~~ Int, 'IntStr.new ~~ Int';
    ok $x ~~ Str, 'IntStr.new ~~ Str';
    ok $x ~~ IntStr, 'IntStr.new ~~ IntStr';
    is +$x, 42, '+IntStr.new is 42';
    is ~$x, 'forty-two', '~IntStr.new is "forty-two"';
}

# NumStr.new constructor
{
    my $x = NumStr.new(3.14e0, "pi-ish");
    is $x.WHAT.gist, '(NumStr)', 'NumStr.new.WHAT is NumStr';
    ok $x ~~ Num, 'NumStr.new ~~ Num';
    ok $x ~~ Str, 'NumStr.new ~~ Str';
    is +$x, 3.14, '+NumStr.new is 3.14';
    is ~$x, 'pi-ish', '~NumStr.new is "pi-ish"';
}

# RatStr.new constructor
{
    my $x = RatStr.new(2/3, "two-thirds");
    is $x.WHAT.gist, '(RatStr)', 'RatStr.new.WHAT is RatStr';
    ok $x ~~ Rat, 'RatStr.new ~~ Rat';
    ok $x ~~ Str, 'RatStr.new ~~ Str';
    is ~$x, 'two-thirds', '~RatStr.new is "two-thirds"';
}

# Non-allomorphic types should not match Allomorph
{
    nok 42 ~~ Allomorph, 'plain Int !~~ Allomorph';
    nok "hello" ~~ Allomorph, 'plain Str !~~ Allomorph';
    nok 3.14 ~~ Allomorph, 'plain Rat !~~ Allomorph';
}

# val() function
{
    is val("42").WHAT.gist, '(IntStr)', 'val("42") is IntStr';
    is val("3.14").WHAT.gist, '(RatStr)', 'val("3.14") is RatStr';
    is val("hello").WHAT.gist, '(Str)', 'val("hello") is plain Str';
}
