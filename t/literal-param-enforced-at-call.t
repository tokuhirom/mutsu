use v6;
use Test;

plan 12;

# A literal parameter (`sub f("a") { }`, `-> 'about' { }`) constrains the
# argument: raku throws X::TypeCheck::Binding::Parameter when the argument is
# not that literal. mutsu recorded and introspected the literal correctly but
# the binder ignored it at call time, so any argument bound successfully.
# (todo/tickets/literal-parameters-are-not-enforced-at-bind.md)

{
    sub f("a") { "hit" }
    is f("a"), "hit", 'a matching literal argument still binds';
    dies-ok { f("b") }, 'a non-matching literal argument dies';

    try f("b");
    my $ex = $!;
    isa-ok $ex, 'X::TypeCheck::Binding::Parameter',
        'the thrown exception is X::TypeCheck::Binding::Parameter';
    is $ex.message,
        q{Constraint type check failed in binding to parameter '<anon>'; expected "a" but got "b"},
        'the message matches raku wording exactly';
    is $ex.expected, "a", '.expected is the literal value itself';
    is $ex.got, "b", '.got is the offending argument value';
}

{
    my $f = -> 'about' { "hit" };
    is $f("about"), "hit", 'a matching literal pointy-block argument still binds';
    dies-ok { $f("nope") }, 'a non-matching literal pointy-block argument dies';
}

{
    sub g(0) { "zero" }
    is g(0), "zero", 'an Int literal parameter matches its value';
    dies-ok { g(1) }, 'and rejects a different Int';
}

# Multi dispatch must keep skipping a non-matching literal candidate in favour
# of the next one, rather than dying at bind time -- the enforcement above
# only fires for a call that actually commits to the literal candidate.
{
    multi sub h("a") { "got-a" }
    multi sub h($x) { "got-other: $x" }
    is h("a"), "got-a", 'multi dispatch still picks the literal candidate';
    is h("z"), "got-other: z", 'and still falls back when the literal does not match';
}
