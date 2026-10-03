use Test;

plan 7;

# A custom attribute trait may re-dispatch to CORE's own `trait_mod:<is>`
# candidates (`:rw`, `:built`), as HTML::Component's `is html-attr` does with
# `trait_mod:<is>($attr, :built)`. The result is the same as writing the core
# trait on the `has` line.
role Marked { }

multi trait_mod:<is>(Attribute $a, :$marked-built!) {
    trait_mod:<is>($a, :built);
    $a does Marked;
}
multi trait_mod:<is>(Attribute $a, :$marked-rw!) {
    trait_mod:<is>($a, :rw);
}
multi trait_mod:<is>(Attribute $a, :$unbuilt!) {
    trait_mod:<is>($a, :built(False));
}

class C {
    has $.pub   is marked-built;
    has $!priv  is marked-built;
    has $.w     is marked-rw;
    has $.never is unbuilt;
    method priv { $!priv }
}

my $c = C.new(pub => 1, priv => 2, never => 3);
is $c.pub, 1, 'a public attribute stays built';
is $c.priv, 2, ':built makes a private attribute initializable from .new';
ok C.^attributes.first(*.name eq '$!pub') ~~ Marked, 'the role mixed in by the trait is kept';
nok $c.never.defined, ':built(False) stops .new initializing a public attribute';

$c.w = 42;
is $c.w, 42, ':rw makes the accessor writable';

# The same re-dispatch inside a role's attribute applies to the consuming class.
role R {
    has $!inner is marked-built;
    method inner { $!inner }
}
class D does R { }
is D.new(inner => 'x').inner, 'x', 'a role attribute re-dispatching to :built';

dies-ok { trait_mod:<is>(C.^attributes[0], :no-such-core-trait) },
    'a named argument no candidate takes still fails to dispatch';
