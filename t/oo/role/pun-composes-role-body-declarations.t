use Test;

# Punning a role -- calling a method on the role TYPE OBJECT, or tying a
# container to it with `is` -- is a composition, so the role's body runs and
# its body lexicals are that pun's own scope.
#
# mutsu dispatched a method straight off the un-punned role, so the body never
# ran at all: a body `my $x` read back as Nil and a body `my class` was
# "Undeclared name". Found via the Hash::Ordered / Hash::Agnostic zef
# distribution, whose `method kv` returns `Seq.new(KV.new(...))` over a
# `my class KV does Iterator` declared in the role body.

plan 8;

role PlainLexical {
    my $answer = 42;
    method ans() { $answer }
}
is PlainLexical.ans, 42, 'a body `my` scalar is visible from a punned role type object';
is PlainLexical.new.ans, 42, 'and from an instance of the pun';

role BodyType {
    my class KV { method greet() { 'from KV' } }
    method kv() { KV.new.greet }
}
is BodyType.kv, 'from KV', 'a body `my class` resolves from a punned role type object';
is BodyType.new.kv, 'from KV', 'and from an instance of the pun';

# The declaration may live in a role the punned role only composes.
role Parent {
    my class Inner { method who() { 'inner' } }
    my $parent-lex = 'parent-lex';
    method inner() { Inner.new.who }
    method plex()  { $parent-lex }
}
role Child does Parent { }
is Child.inner, 'inner', 'a composed parent role body type reaches the pun';
is Child.plex, 'parent-lex', 'a composed parent role body lexical reaches the pun';

# A class consumer has always worked; keep it pinned so the two routes cannot
# drift apart again.
class Consumer does Parent { }
is Consumer.inner, 'inner', 'a class consumer still sees the role body type';

# The `is <role>` container trait puns too, and the method may be reached only
# through the parent role -- the distribution's exact shape.
role Assoc does Associative {
    my class Marker { method tag() { 'marked' } }
    method AT-KEY($k) { $k.uc }
    method keys()     { <a b> }
    method marker()   { Marker.new.tag }
}
role Tied does Assoc { }
my %h is Tied;
is %h.marker, 'marked', 'a tied container reaches a grandparent role body type';

# NOT pinned here: a role body's assignment to an OUTER lexical
# (`my $side; role R { $side = 1 }; R.new; say $side` -- rakudo answers 1)
# still does not reach the enclosing scope under mutsu. The body does run
# (everything above proves it); the write is lost on the way out, which is the
# locals/env dual store, not this fix. Tracked separately.
