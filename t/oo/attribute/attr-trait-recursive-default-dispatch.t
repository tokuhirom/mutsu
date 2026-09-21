use Test;

plan 3;

# A distribution's own custom attribute trait may re-dispatch to CORE's own
# `trait_mod:<is>(Attribute, :$default!)` candidate as an ordinary function
# call from inside its own handler, to reuse `is default(...)`'s semantics on
# an `Attribute` object it already holds. This is the pattern ASN::BER's
# ASN::Types module uses for its own `is default-value(...)` trait:
# `trait_mod:<is>($attr, :default($default-value))`. mutsu only recognized
# `is default(...)` written directly on a `has` line (compile-time sugar),
# so the runtime call found no candidate and the "no candidate" verdict was
# misreported as the OUTER `is default-value` trait itself being unknown.

role Marker {}
multi trait_mod:<is>(Attribute $attr, :$marked!) {
    $attr does Marker;
}

role DefaultValue[:$default-value] {
    method default-value() { $default-value }
}
multi trait_mod:<is>(Attribute $attr, :$default-value!) {
    $attr does DefaultValue[:$default-value];
    trait_mod:<is>($attr, :default($default-value));
}

class Rocket {
    has Str $.message is marked is default-value("Hello World");
}

my $r1 = Rocket.new;
is $r1.message, 'Hello World', 'runtime-dispatched default applies when the attribute is omitted';

my $r2 = Rocket.new(message => 'Falcon');
is $r2.message, 'Falcon', 'explicit value overrides the runtime-set default';

is Rocket.^attributes.first(*.name eq '$!message').default-value, 'Hello World', 'the custom trait state is still reachable';

# vim: expandtab shiftwidth=4
