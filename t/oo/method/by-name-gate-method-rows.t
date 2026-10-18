use Test;

# The by-name gates of `call_method_with_values` that became rows
# (ADR-11276 §9.164). Expected values are Rakudo's.

plan 44;

# Regex.ACCEPTS answers the Match, not a Bool.
{
    my $rx = /b+/;
    is $rx.ACCEPTS("abbc"), "bb", 'Regex.ACCEPTS answers the Match';
    isa-ok $rx.ACCEPTS("abbc"), Match, '... of class Match';
    nok $rx.ACCEPTS("xyz").defined, 'Regex.ACCEPTS of a miss is Nil';
    is rx:i/B/.ACCEPTS("abc"), "b", 'an adverbed regex too';
    my $m = "ACCEPTS";
    is $rx."$m"("abbbc"), "bbb", 'a runtime method name reaches the row';
}

# Code.ACCEPTS calls the code; WhateverCode.ACCEPTS answers its truth.
{
    is (-> $x { $x * 2 }).ACCEPTS(4), 8, 'Code.ACCEPTS answers the call';
    is { 42 }.ACCEPTS(4), 42, 'a block that takes no argument is called without it';
    is (-> $x { $x }).ACCEPTS(0), 0, 'the result is not a Bool';
    sub triple($x) { $x * 3 }
    is &triple.ACCEPTS(2), 6, 'a named routine';
    my $w = * > 3;
    is-deeply $w.ACCEPTS(4), True, 'WhateverCode.ACCEPTS is the truth';
    is-deeply $w.ACCEPTS(1), False, '... both ways';
}

# Signature.ACCEPTS is the bind test.
{
    sub f(Int $a, Str $b) { }
    is-deeply &f.signature.ACCEPTS(\(1, "x")), True, 'Signature.ACCEPTS binds';
    is-deeply &f.signature.ACCEPTS(\(1, 2)), False, '... or does not';
    is-deeply :(Int $x).ACCEPTS(\(3)), True, 'a signature literal';
    is &f.signature.arity, 2, 'Signature.arity';
    is :($a, *@b).count, Inf, 'Signature.count with a slurpy';
}

# Mu.ACCEPTS of a type object is the type check.
{
    is-deeply Int.ACCEPTS(5), True, 'Int.ACCEPTS(5)';
    is-deeply Int.ACCEPTS("x"), False, 'Int.ACCEPTS("x")';
    is-deeply (Int:D).ACCEPTS(Int), False, 'a :D smiley';
    is-deeply (Int:U).ACCEPTS(Int), True, 'a :U smiley';
    is-deeply Range.ACCEPTS(1..2), True, 'a closed shape type object';
    class C { }
    is-deeply C.ACCEPTS(C.new), True, 'a user class';
    is-deeply C.ACCEPTS(3), False, '... both ways';
    class D { method ACCEPTS($x) { "user" } }
    is D.ACCEPTS(3), "user", "a class's own ACCEPTS wins";
    role Weird { multi method ACCEPTS($x) { "weird $x" } }
    enum E does Weird <a b>;
    is E.ACCEPTS(4), "weird 4", "an enum's composed role ACCEPTS wins";
    subset Even of Int where * %% 2;
    is-deeply Even.ACCEPTS(4), True, 'a subset';
    is-deeply Even.ACCEPTS(3), False, '... both ways';
}

# IO::Handle's `nl-out` and `chomp` of a subclass with no handle.
{
    class H is IO::Handle { }
    is H.new.nl-out, "\n", 'nl-out defaults to a newline';
    is-deeply H.new.chomp, True, 'chomp defaults to True';
    class S is Str { }
    is S.new(value => "ab\n").chomp, "ab", 'a Str subclass keeps Str.chomp';
}

# `self`, `sum` and `hash` with arguments.
{
    is 5.self(:a), 5, 'Mu.self takes a named argument';
    throws-like { 5.self(1, 2) }, Exception, 'but no positional one';
    is [1, 2, 3].sum(:wrap), 6, 'sum takes an adverb';
    is (1, 2, 3).sum(:wrap), 6, '... on a List';
    is-deeply (a => 1, b => 2).hash, {a => 1, b => 2}, 'List.hash';
    is-deeply (1, 2).Slip.Hash, {"1" => 2}, 'Slip.Hash';
}

# `Range` refuses the mutators; `Any`'s growing methods refuse a defined
# scalar and autovivify an undefined receiver.
{
    throws-like { (1..3).push(4) }, X::Immutable, 'Range.push is X::Immutable';
    throws-like { (1..3).shift }, X::Immutable, 'Range.shift too';
    throws-like { (1..3).splice(1) }, X::Multi::NoMatch, 'Range.splice has no candidate';
    throws-like { 5.push(4) }, X::Multi::NoMatch, 'Int.push has no candidate';
    throws-like { 5.pop }, X::Method::NotFound, 'Int.pop is no method';
    throws-like { "x".subst-mutate("x", "y") }, X::Multi::NoMatch,
        'subst-mutate of a literal has no candidate';
    my @a;
    @a[2].unshift(5);
    is-deeply @a[2], [5], 'an undefined element autovivifies';
    my %h;
    %h<a>.append(1, 2);
    is-deeply %h<a>, [1, 2], 'a hash entry too';
}
