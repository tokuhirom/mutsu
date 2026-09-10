use Test;

# `dispatch_bless` seeds every attribute the caller did not supply. That seed —
# a native zero, or the attribute's nominal type object — is a pure function of
# the class shape, so it is precomputed once per class in `NativeCtorPlan`
# rather than re-derived (a type-constraint lookup, a nominal-type walk and a
# `Symbol::intern`) on every construction. Same for the named-argument →
# attribute-index lookup, which is a hash probe rather than a linear name scan.
plan 12;

subset Small of Int where * < 10;

class Seeds {
    has $.untyped;
    has Int $.typed;
    has Str $.stringy;
    has Small $.subsetted;
    has int $.native-int;
    has num $.native-num;
    has str $.native-str;
    has @.list;
    has %.map;
    has $.given;

    method new(*%_) { self.bless(|%_) }
}

my $s = Seeds.new(given => 42);

is $s.untyped.gist, "(Any)", "an untyped attribute seeds Any";
is $s.typed.gist, "(Int)", "a typed attribute seeds its declared type object";
is $s.stringy.gist, "(Str)", "Str-typed attribute seeds Str";
is $s.subsetted.gist, "(Small)", "a subset-typed attribute seeds the subset type object";
is $s.native-int, 0, "a native int attribute seeds 0";
is $s.native-num, 0e0, "a native num attribute seeds 0e0";
is $s.native-str, "", "a native str attribute seeds the empty string";
is-deeply $s.list, [], "an untouched \@ attribute seeds an empty Array";
is-deeply $s.map, {}, "an untouched \% attribute seeds an empty Hash";
is $s.given, 42, "a supplied named argument still wins over the seed";

# A named argument that matches no declared attribute is dropped (raku's
# default BUILDALL ignores it), and the index lookup must not confuse it for
# one that does.
my $t = Seeds.new(typed => 3, nosuchthing => 9);
is $t.typed, 3, "the matching named argument lands on its attribute";

# A child re-declaring a parent attribute name: the bless override must pick
# the same attribute the seed loop filled, not a later same-named entry.
class Parent { has $.dup }
class Child is Parent {
    has $.own;
    method new(*%_) { self.bless(|%_) }
}
is Child.new(dup => 7, own => 8).dup, 7, "a re-declared attribute name resolves to one slot";
