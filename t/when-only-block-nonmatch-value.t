use Test;

plan 11;

# Rakudo rule: a block whose tail when-chain matches nothing evaluates to
# the failed test's falsy value — Int 0 for a type-object matcher, Bool::False
# otherwise. Never the topic, never Nil/Empty.
# (todo/tickets/when-only-block-nonmatch-value-wrong.md)

is (1, "a", 2).map({ when Int { "int" } }).join(","), "int,0,int",
    'map: non-matching item yields Int 0 for a type matcher';

is (1..5).map({ when 2 { "two" } }).join(","), "False,two,False,False,False",
    'map: non-matching item yields False for a value matcher';

is (1, "a", 2, "b").grep({ when Int { True } }).join(","), "1,2",
    'grep: when-only block filters items matching no branch';

is (1..5).grep({ when 2 { True } }).join(","), "2",
    'grep: value-matcher when-only block filters too';

nok (1, 2, 3).first({ when 5 { True } }).defined,
    'first: a when-only predicate matching nothing finds nothing';

is (1, "a").map({ when Int { "int" }; default { "other" } }).join(","), "int,other",
    'map: a default branch supplies the non-match value instead';

is (2, 3).map({ my $x = 42; when 2 { "two" } }).join(","), "two,False",
    'map: a statement before the tail when does not change the rule';

is (2, 3).map({ when 2 { "two" }; "after" }).join(","), "two,after",
    'map: a when followed by another statement falls through to it';

{
    my @out;
    for 1..4 { when 2 { @out.push("two") } }
    is @out.join(","), "two", 'for: when-only loop body still fires only on the match';
}

{
    my @a = (1, 2, 3);
    @a .= map({ when Int { $_ * 10 } });
    is @a.join(","), "10,20,30", 'rw .=map with a matching when still writes back';
}

todo "direct block call goes through the closure-call fallback, not the map/grep fast path; needs the general exec_when_op fix (see todo/tickets/when-nonmatch-value-outside-map-grep.md)";
is-deeply { when 2 { "two" } }(3), False,
    'direct call: non-matching when-only block evaluates to False';

done-testing;
