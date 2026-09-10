use Test;

plan 25;

# ADR-0052: a `when`/`default` clause leaves exactly one value on the VM stack
# on BOTH branches, and that stack value is the only transport of the clause's
# value. Before this, the non-match value reached only the four inlined
# `.map`/`.grep`/`.first` fast paths (through an interpreter-global side
# channel); every other consumer read the block's compiled return value and got
# Nil/Any.

# -- The three origin probes (todo/deep/when-nonmatch-value-outside-map-grep.md)

{
    my $b = { when 2 { "two" } };
    is-deeply $b(3), False, 'probe 7: direct closure call, non-matching when';
}

{
    $_ = False;
    my $a = do { when .so { "foo" } };
    is-deeply $a, False, 'probe 31: do-block with a non-matching when tail';
}

is-deeply (given 3 { when 2 { "two" } }), False,
    'probe 32: given-expression whose when does not match';

# A routine body whose tail is a when-chain: `$_` is undefined there, so the
# literal matcher lowers to the Int 0 form.
{
    sub h { when 99 { "y" }; when 1 { "m" } }
    is-deeply h(), 0, 'routine tail when-chain, nothing matches';
}

# -- The falsy value is Rakudo's lowering artifact, not the smartmatch result
#
# `(Any ~~ 2)` written as an expression is Bool::False, yet
# `given Any { when 2 {...} }` is Int 0 -- so the value is selected by how the
# matcher was *written*, which is why the clause carries a compile-time
# matcher kind rather than asking the runtime comparison.

is-deeply (Any,).map({ when 2 { "x" } }).List, (0,),
    'map: literal matcher against a type-object topic is Int 0';
is-deeply (3,).map({ when Str { "x" } }).List, (0,),
    'map: type-object matcher is Int 0 whatever the topic';
is-deeply (3,).map({ when 2 { "x" } }).List, (False,),
    'map: literal matcher against a defined topic is Bool::False';

is-deeply (given 3 { when Str { "m" } }), 0, 'type-object matcher: Int 0';
is-deeply (given 3 { when Nil { "m" } }), 0, 'Nil matcher is a type object too';
is-deeply (given Any { when "y" { "m" } }), 0, 'Str literal, undefined topic';
is-deeply (given Int { when Str { "m" } }), 0, 'both undefined';
is-deeply (given "x" { when "y" { "m" } }), False, 'Str literal, defined topic';
is-deeply (given 3 { when 1..2 { "m" } }), False, 'a Range matcher is computed';
is-deeply (given Any { when /x/ { "m" } }), False, 'a regex matcher is computed';
is-deeply (given 3 { when { $_ > 9 } { "m" } }), False, 'a block matcher is computed';

{
    my $mt = Str;
    is-deeply (given 3 { when $mt { "m" } }), False,
        'a type object reached through a variable is computed, not a type-object matcher';
}

{
    constant C2 = 2;
    is-deeply (given Any { when C2 { "m" } }), False,
        'a named constant is not a literal token';
}

# -- The clause's value never comes from the enclosing frame's stack
#
# `exec_when_op` used to PEEK `stack.last()` for the matching body's value.
# When the body's tail produced nothing, that peek reached below the clause's
# own stack range and handed out whatever the caller had pushed --
# `say "A: ", (given 2 { when 2 { my $x = 5 } })` printed "A: " for the value.

is-deeply (given 2 { when 2 { my $x = 5 } }), 5,
    'a tail declaration in a when body is the clause value';

# -- Stack hygiene: a non-last clause must not shadow the real tail value

is-deeply (given 2 { when 9 { "w" }; "tail" }), "tail",
    'a non-matching when is popped when it is not the tail';
is-deeply (given 2 { when 9 { "a" }; when 8 { "b" } }), False,
    'the last non-matching when is the given value';
is-deeply (do { when 9 { "a" }; when 8 { "b" }; "t" }), "t",
    'do-block: non-last clauses are popped';

# A matched clause abandons the block, so its value is the block's value even
# with unrelated statements after it.
is-deeply (given 2 { when 2 { "hit" }; "never" }), "hit",
    'a matching when abandons the rest of the block';

# -- A collecting loop keeps the abandoned iteration's value

is-deeply (do for 1..3 { when 2 { "hit" }; "plain" }).List,
    ("plain", "hit", "plain"),
    'do for: a matching iteration collects the clause value';
is-deeply (do for 1..3 { when 2 { "hit" } }).List, (False, "hit", False),
    'do for: non-matching iterations collect the falsy value';

# -- The postfix `STMT when COND` spelling is not a clause
#
# Rakudo lowers it to a plain conditional, so a false one is `Empty`, not the
# clause falsy value. `exec_when_op` already knows which spelling it is.
{
    $_ = 7;
    my $wm = ("a" when 0);
    is-deeply $wm, Empty, 'a false when-modifier yields Empty, not the falsy value';
}

done-testing;
