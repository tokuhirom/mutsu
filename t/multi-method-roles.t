use Test;

plan 13;

# Multi method dispatch on roles mixed in with `does`, with attribute writeback.
{
    role R5 {
        multi method rt()       { push @.order, 'empty' }
        multi method rt(Str $a) { push @.order, 'Str'   }
    }
    role R6 {
        multi method rt(Numeric $a) { push @.order, 'Numeric' }
    }
    class C { has @.order }

    my C $b1 .= new();
    $b1 does (R5, R6);
    $b1.*rt;
    is $b1.order, <empty>, 'multi method .* dispatch on empty signature';

    my C $b2 .= new();
    $b2 does (R5, R6);
    $b2.*rt('hi');
    is $b2.order, <Str>, 'multi method .* dispatch on Str signature';

    my C $b3 .= new();
    $b3 does (R5, R6);
    $b3.*rt(42);
    is $b3.order, <Numeric>, 'multi method .* dispatch on Numeric signature';
}

# A normal (single-dispatch) role method should also propagate attribute
# mutations on the inner class instance back through the Mixin.
{
    role Adder {
        method add($x) { push @.items, $x }
    }
    class Box { has @.items }
    my Box $b .= new();
    $b does Adder;
    $b.add(1);
    $b.add(2);
    is $b.items, [1, 2], 'role method mutates inner class @-attribute through Mixin';
}

# `@.attr` push writeback inside a plain class method.
{
    class Acc {
        has @.log;
        method note-it($x) { push @.log, $x }
        method note2($x)   { @.log.push($x) }
    }
    my $a = Acc.new;
    $a.note-it('a');
    $a.note2('b');
    is $a.log, ['a', 'b'], 'push @.attr and @.attr.push both write back';
}

# .candidates on a multi method returns one Method per candidate.
{
    role RS { multi method d(Str $x) { 'string' } }
    role RI { multi method d(Int $x) { 'integer' } }
    class M does RS does RI {
        multi method d(Any $x) { 'any' }
    }
    my $m = M.new;
    is $m.d(876), 'integer', 'dispatch to Int role candidate';
    is $m.d('7'), 'string',  'dispatch to Str role candidate';
    is $m.d(1.2), 'any',     'dispatch to Any class candidate';

    my @mm = $m.^methods.grep({ .name eq 'd' });
    is @mm.elems, 1, '^methods returns one element for a multi';
    my @cands = @mm[0].candidates;
    is @cands.elems, 3, 'multi method has three candidates';
    ok @cands[0] ~~ Method, 'candidate is a Method';
}

# Ambiguous multi dispatch dies.
{
    class Tie {
        multi method t(Int $x) { 1 }
        multi method t(Int $y) { 2 }
    }
    dies-ok { Tie.t(42) }, 'ambiguous multi dispatch dies';
}

# multi submethod can be declared and called.
{
    my class S {
        multi submethod foo(Str $a) { 'specific' }
    }
    is S.new.foo('hi'), 'specific', 'multi submethod callable';
}
