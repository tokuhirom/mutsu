use v6;
use Test;

plan 39;

# ---------------------------------------------------------------------------
# Metamodel::TypePretense: a role type object pretends to be Cool/Any/Mu.
# ---------------------------------------------------------------------------
{
    role PretenseRole { }

    ok PretenseRole ~~ Mu,   'a role type object type-checks against Mu';
    ok PretenseRole ~~ Any,  'a role type object type-checks against Any';
    ok PretenseRole ~~ Cool, 'a role type object type-checks against Cool';
    nok PretenseRole ~~ Str, 'the pretense chain stops at Cool (not Str)';

    is-deeply PretenseRole.HOW.pretending_to_be.map(*.^name).List,
        ('Cool', 'Any', 'Mu'),
        '.HOW.pretending_to_be lists the pretended chain';
    role Curried[::T] { }
    ok Curried[Int] ~~ Cool, 'a curried role pretends to be Cool too';
    is-deeply Curried[Int].HOW.pretending_to_be.map(*.^name).List,
        ('Cool', 'Any', 'Mu'),
        'a curried role answers .pretending_to_be';

    class PlainClass { }
    dies-ok { PlainClass.HOW.pretending_to_be },
        'a ClassHOW has no pretending_to_be';
}

# ---------------------------------------------------------------------------
# Core roles report ParametricRoleGroupHOW, not ClassHOW.
# ---------------------------------------------------------------------------
{
    is Positional.HOW.^name, 'Perl6::Metamodel::ParametricRoleGroupHOW',
        'Positional is a role';
    is Blob.HOW.^name, 'Perl6::Metamodel::ParametricRoleGroupHOW',
        'Blob is a role';
    is Sequence.HOW.^name, 'Perl6::Metamodel::ParametricRoleGroupHOW',
        'Sequence is a role';
    is PositionalBindFailover.HOW.^name,
        'Perl6::Metamodel::ParametricRoleGroupHOW',
        'PositionalBindFailover is a role';
}

# ---------------------------------------------------------------------------
# A class may compose the natively-modelled core roles.
# ---------------------------------------------------------------------------
{
    my $composed = 0;
    lives-ok {
        EVAL 'class BindFailoverConsumer does PositionalBindFailover {
                  method iterator { (1, 2, 3).iterator }
              }';
        $composed = 1;
    }, 'class ... does PositionalBindFailover composes';
    is $composed, 1, 'the composing class registered';
    ok EVAL('BindFailoverConsumer.new') ~~ PositionalBindFailover,
        'an instance does PositionalBindFailover';

    lives-ok { EVAL 'class SequenceConsumer does Sequence { }' },
        'class ... does Sequence composes';
    lives-ok { EVAL 'class QuantHashConsumer does QuantHash { }' },
        'class ... does QuantHash composes';
}

# ---------------------------------------------------------------------------
# A role parameter's default is evaluated when the role is composed with no
# explicit arguments -- including through the `does` mixin operator.
# ---------------------------------------------------------------------------
{
    role Defaulted[$p = 5] { method p { $p } }

    my $mixed = 1 does Defaulted;
    is $mixed.^name, 'Int+{Defaulted}',
        'an unparameterised mixin keeps the plain composed name';
    is $mixed.p, 5, 'the role parameter is bound to its default';

    class DefaultedConsumer does Defaulted { }
    is DefaultedConsumer.new.p, 5, 'a class-header composition binds it too';
    is Defaulted.new.p, 5, 'the pun binds it too';

    role CaptureDefaulted[::T = Int] { method t { T } }
    my $captured = 1 does CaptureDefaulted;
    is $captured.t.^name, 'Int', 'a defaulted TYPE capture binds too';

    role Boom[$p = fail("boom")] { }
    throws-like { my $x = 1 does Boom }, X::Role::Instantiation,
        'a failing default rejects a `does` composition';
    throws-like { EVAL 'class BoomConsumer does Boom { }' },
        X::Role::Instantiation,
        'a failing default rejects a class-header composition';
}

# ---------------------------------------------------------------------------
# A role's own method outranks the accessor of the role's own attribute,
# whether or not the composing class has a body of its own.
# ---------------------------------------------------------------------------
{
    role Notable {
        has Str $.notes is rw;
        multi method notes()          { "[$!notes]" }
        multi method notes(Str $note) { $!notes ~= $note }
    }

    class BareConsumer does Notable { }
    my $bare = BareConsumer.new;
    $bare.notes("a");
    is $bare.notes, '[a]', 'role method wins for a body-less class';

    class AttrConsumer does Notable { has $.extra; }
    my $attr = AttrConsumer.new;
    $attr.notes("a");
    is $attr.notes, '[a]', 'role method wins when the class adds an attribute';

    class MethodConsumer does Notable { method extra { 1 } }
    my $meth = MethodConsumer.new;
    $meth.notes("a");
    is $meth.notes, '[a]', 'role method wins when the class adds a method';

    # ... but a CLASS-declared attribute still outranks a role method
    # ("class prioritization").
    role JustMethod { method n { 'from-role' } }
    class OwnAttr does JustMethod { has $.n }
    nok OwnAttr.new.n.defined,
        'a class-declared attribute still outranks a role method';
}

# ---------------------------------------------------------------------------
# `::?ROLE:D` / `::?CLASS:D` constrain the FOLLOWING parameter, including a
# nested colonpair-alias named parameter.
# ---------------------------------------------------------------------------
{
    my $loaded = 0;
    lives-ok {
        EVAL 'role MetaTyped { method create(::?ROLE:D :from(:$for)!) { $for } }';
        $loaded = 1;
    }, 'a ::?ROLE:D-constrained nested colonpair param declares cleanly';
    is $loaded, 1, 'the role body ran to completion';

    class Aliased {
        method create(::?CLASS:D :from(:$for)!) { $for }
    }
    my $inst = Aliased.new;
    is Aliased.create(from => $inst), $inst, 'the `from` spelling binds';
    is Aliased.create(for  => $inst), $inst, 'the `for` alias binds too';
}

# ---------------------------------------------------------------------------
# A parametric role with a self-referential attribute type.
# ---------------------------------------------------------------------------
{
    role Box[::Type] {
        has Box[Type] $.child;
        has Type      $.val;
    }

    my $b = Box[Int].new(val => 1);
    is $b.^name, 'Box[Int]', 'the pun is named for its parameterisation';
    is $b.val, 1, 'a plain type-parameter attribute binds';
    is $b.child.^name, 'Box[Int]',
        'an unset self-referential attribute defaults to the CONCRETE type object';
    nok $b.child.defined, 'and it is a type object, not an instance';

    my $outer = Box[Int].new(val => 2, child => Box[Int].new(val => 3));
    is $outer.child.val, 3, 'a supplied self-referential child round-trips';
}

# ---------------------------------------------------------------------------
# An anonymous role mixed into a value renders with a generated id, so assert
# the SHAPE of the name rather than the id itself.
# ---------------------------------------------------------------------------
{
    my @a = 1, 2, 3;
    my $mixed = @a but role { method tagged { 'yes' } };
    like $mixed.^name, /^Array\+\{/, 'an anonymous role mixin renders as Array+{...}';
    is $mixed.tagged, 'yes', 'the anonymous role method is reachable';
}

# vim: expandtab shiftwidth=4
