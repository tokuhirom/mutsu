use v6;
use Test;

plan 7;

{
    # `y` ends its declarative prefix in a fate (a code block) after 'abc', so
    # it ranks first (prefix 3) and then fails to match. 'a' and 'ab' are
    # sound prefixes of 1 and 2.
    my $m = 'abcd' ~~ / [ 'a' | 'abc' {} 'X' | 'ab' ] /;
    is ~$m, 'ab', "a branch's fate is not another branch's";
}

{
    # All three reach 4 characters; the longest literal prefix breaks the tie.
    my @ran;
    'abcd' ~~ / [ <[a..z]> ** 4 { @ran.push('w') } | 'ab' <[a..z]> ** 2 { @ran.push('l') } | 'abcd' { @ran.push('x') } ] /;
    is-deeply @ran, ['x'], "a branch's literals are counted for that branch only";
}

{
    grammar Shared {
        token TOP { [ <w> 'x' | <w> 'y' <w> ] }
        token w { \w }
    }
    is ~Shared.parse('ayz'), 'ayz', 'branches that call the same rule are ranked separately';
    is ~Shared.parse('ax'), 'ax', 'the other branch wins where it is the longer';
}

{
    # The rule a branch calls resolves in the package the match runs in.
    grammar A {
        token TOP { [ <x> 'a' | <x> 'b' ] }
        token x { 'p' }
    }
    grammar B is A {
        token x { 'q' }
    }
    is ~A.parse('pb'), 'pb', 'the base grammar ranks with its own rule';
    is ~B.parse('qb'), 'qb', 'a derived grammar ranks with the overridden rule';
}

{
    # A branch that cannot match here is ranked last, not dropped.
    my $m = 'ab' ~~ / [ 'zzz' | 'ab' ] /;
    is ~$m, 'ab', 'a branch that cannot match still leaves the rest in rank order';
}
