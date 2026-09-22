use Test;

# Found via Graph::Star's `multi method new(Int:D :leaves(:rays(:$n)), ...)`:
# a named parameter's aliases can nest arbitrarily deep (`:leaves(:rays(:$n))`
# is TWO levels — `leaves` aliasing `rays` aliasing `n`), not just one.
# Binding already walked the whole chain, but multi-candidate *matching* only
# ever collected the outer alias's immediate children
# (`ParamDef::named_external_keys`), so a call spelled with the innermost
# alias (`n => 5`, the ecosystem call site's own spelling) never matched the
# signature at all and fell through to a less-specific ancestor candidate
# instead — `Graph::Star.new(n => 5, ...)` silently constructed a bare
# `Graph` missing its own required `$.n` attribute.

plan 4;

class C {
    multi method make(Int:D :leaves(:rays(:$n)), :$prefix = '') {
        "n=$n prefix=$prefix";
    }
}
is C.new.make(n => 5, prefix => 'x'), 'n=5 prefix=x',
    'the innermost alias of a two-level named-param alias chain matches';
is C.new.make(rays => 5, prefix => 'x'), 'n=5 prefix=x',
    'the middle alias matches too';
is C.new.make(leaves => 5, prefix => 'x'), 'n=5 prefix=x',
    'and the outer (primary) name still matches';

# The alias must not weaken candidate selection: a genuinely unrelated
# ancestor candidate must not win just because the nested-alias candidate
# was wrongly treated as inapplicable.
class Base {
    has %.stuff;
    submethod BUILD(:%!stuff = %()) { }
    multi method new(:%stuff = %()) { self.bless(:%stuff) }
}
class Star is Base {
    has Int:D $.n is required;
    submethod BUILD(:$!n!) { }
    multi method new(Int:D :leaves(:rays(:$n))) {
        self.bless(:$n);
    }
}
is Star.new(n => 5).n, 5,
    'a nested-alias constructor reaches its own class, not an ancestor default';

done-testing;
