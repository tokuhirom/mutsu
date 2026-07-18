use v6;
use Test;

plan 6;

# An @-sigil attribute bound (`:=`) to a List keeps List-ness (URI::Path).
class WithSegments {
    has @.segments;
    submethod BUILD(:@segments = ()) {
        @!segments := @segments.List;
    }
}

my $p = WithSegments.new(segments => ('', 'a', 'b'));
isa-ok $p.segments, List, 'bound .List attribute is a List';
is-deeply $p.segments, $('', 'a', 'b'), 'is-deeply sees a List, not an Array';

# Plain assignment still coerces to Array.
class WithArray {
    has @.items;
    submethod BUILD(:@items = ()) {
        @!items = @items.List;
    }
}
isa-ok WithArray.new(items => (1, 2)).items, Array, 'plain = assignment stays Array';

# %-sigil bind keeps Map (regression guard for the sibling branch).
class WithMap {
    has %.h;
    submethod BUILD(:%h = {}) {
        %!h := %h.Map;
    }
}
isa-ok WithMap.new(h => {a => 1}).h, Map, 'bound .Map attribute is a Map';

# Binding a non-Positional to an @ attribute dies with X::TypeCheck::Binding.
class BadBind {
    has @.a;
    method go() { @!a := 42 }
}
throws-like { BadBind.new.go }, X::TypeCheck::Binding,
    'binding a non-Positional to @!attr throws X::TypeCheck::Binding';

# `my @x := <list>` (SetLocal path) still preserves List.
my @x := (1, 2, 3);
isa-ok @x, List, 'my @x := (1,2,3) stays a List';
