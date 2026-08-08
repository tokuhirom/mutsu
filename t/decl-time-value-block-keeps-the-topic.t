use Test;

plan 5;

# A declaration-time expression (an attribute's type constraint, an attribute
# default, an enum value, a role argument) is evaluated by compiling it as a
# block *for its value*, which makes its last expression a `SetTopic`. That runs
# inside whatever frame is constructing, so the topic write escaped to the
# caller.

{
    class S { has Bool $.b }
    $_ = 3.7e0;
    S.new;
    is $_.^name, 'Num', 'constructing with an unset typed attribute keeps the topic';
}

{
    class T { has Int $.i is rw; has Rat $.r is rw }
    $_ = 3.7e0;
    T.new(i => 1);
    is $_.^name, 'Num', 'several typed attributes, one of them supplied';
}

{
    class U { has Int $.i is rw = 5 }
    $_ = 3.7e0;
    U.new;
    is $_.^name, 'Num', 'an attribute default keeps the topic too';
}

# The shape Cro hit: the topic is a loop variable and the constructed object's
# attributes are typed, so the next statement saw the attribute's type object
# instead of the loop item.
{
    class State { has Str $.name is rw; has Bool $.flag is rw }
    my @seen;
    for <a b> {
        my $st = State.new(name => 'x');
        @seen.push: $_;
    }
    is-deeply @seen, ['a', 'b'], 'a loop topic survives constructing a typed object';
}

# The value the declaration-time evaluation produces is still correct.
{
    class V { has Int $.i is rw }
    nok V.new.i.defined, 'the unset typed attribute is still its own type object';
}
