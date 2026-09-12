use lib 't/lib';
use Test;

# `also is Parent` executes at its POSITION IN THE CLASS BODY, so everything the
# body established before that line is visible to it (issue #8099). mutsu's
# parser hoists the statement onto the declaration's parent list, which used to
# force the parent to resolve before the body had run at all -- so a parent the
# body itself introduced died as X::Inheritance::UnknownParent. Two real shapes
# hit this: `Font::AFM`'s 14 `Font::Metrics::*` compunits (`use` inside the
# body) and `Intl::CLDR`'s five `CLDR::*` format systems (a class the body
# declares).

plan 17;

# (a) the parent arrives from a `use` inside the body.
{
    class Kid {
        use AlsoIsBodyUseParent;
        also is AlsoIsBodyUseParent;
    }
    is Kid.new.greet, 'from-parent', 'also is a parent `use`d inside the body inherits';
    # The `use` is lexical to the class body, so name the parent through the MRO
    # rather than referring to it out here.
    is Kid.^mro.map(*.^name).join(','), 'Kid,AlsoIsBodyUseParent,Any,Mu',
        'the deferred parent is a real ancestor';
}

# (b) the parent is a class the body itself declares.
{
    class UC2 {
        class Inner { method hello { 'inner' } }
        also is Inner;
    }
    is UC2.^mro.map(*.^name).join(','), 'UC2,UC2::Inner,Any,Mu',
        'also is a body-declared class resolves in the class own package scope';
    is UC2.new.hello, 'inner', 'a method is inherited through the body-declared parent';
}

# The deferred parent carries its own ancestry: attributes and a deeper MRO.
{
    class Base2 { has $.x = 42; method who { 'base2' } }
    class Kid2 {
        class Inner2 is Base2 { method who { 'inner2' } }
        also is Inner2;
        method mine { self.who ~ '/' ~ self.x }
    }
    is Kid2.new.mine, 'inner2/42', 'attributes and methods come through the deferred parent';
    is Kid2.^mro.map(*.^name).join(','), 'Kid2,Kid2::Inner2,Base2,Any,Mu',
        'the deferred parent brings its own ancestors into the C3 MRO';
}

# A deferred name that turns out to be a ROLE is composed, not inherited.
{
    class RKid {
        role Helper { method help { 'helped' } }
        also is Helper;
    }
    is RKid.new.help, 'helped', 'also is a body-declared role composes it';
    is RKid.^roles.map(*.^name).join(','), 'RKid::Helper',
        'the late-composed role is recorded on the class';
}

# A late-composed role does not displace the header`s own composition.
{
    role HeaderRole { method hr { 'hr' } }
    class Mixed does HeaderRole {
        role BodyRole { method br { 'br' } }
        also is BodyRole;
    }
    is Mixed.new.hr ~ Mixed.new.br, 'hrbr', 'header and deferred roles are both composed';
    is Mixed.^roles.map(*.^name).sort.join(','), 'HeaderRole,Mixed::BodyRole',
        'both compositions are recorded';
}

# The two shapes exactly as the affected distributions write them: a `unit
# class`, whose body is the rest of the compilation unit.
{
    use AlsoIsBodyUnitKid;
    is AlsoIsBodyUnitKid.new.extra, 'kid+from-parent',
        'unit class: also is a parent `use`d in the body inherits (the Font::AFM shape)';
    is AlsoIsBodyUnitKid.^mro.map(*.^name).join(','),
        'AlsoIsBodyUnitKid,AlsoIsBodyUseParent,Any,Mu',
        'unit class: the deferred parent lands in the MRO';
}

{
    use AlsoIsBodyUnitNested;
    is AlsoIsBodyUnitNested.^mro.map(*.^name).join(','),
        'AlsoIsBodyUnitNested,AlsoIsBodyUnitNested::Selector,Positional,Any,Mu',
        'unit class: also is a class the body declares (the Intl::CLDR shape)';
    is AlsoIsBodyUnitNested.new[4], 8,
        'the deferred parent brings its own Positional behaviour along';
}

# `also is Foo` on a name that is no type at all is the same thing as `is Foo`
# on one: rakudo's spelling of the named trait argument
# `trait_mod:<is>($type, :Foo)`. So a name the body never introduces falls
# through to that dispatch -- and, being a trait rather than a parent, leaves no
# phantom ancestor behind.
{
    my $seen = 0;
    multi trait_mod:<is>(Mu:U $t, :$MarkedByBody!) { $seen = 1 }
    class Alpha {
        also is MarkedByBody;
    }
    lives-ok { Alpha.new }, 'also is a custom `is` trait still declares the class';
    is Alpha.^mro.map(*.^name).join(','), 'Alpha,Any,Mu',
        'a custom `is` trait named by `also is` is not a C3 parent';
}

# A parent that is still unknown once the body has run is the same error as
# before -- deferral postpones the diagnosis, it does not suppress it.
{
    throws-like 'class StillBad { also is NoSuchParentAnywhere; }',
        X::Inheritance::UnknownParent,
        'a parent the body never introduces still raises X::Inheritance::UnknownParent';
}
