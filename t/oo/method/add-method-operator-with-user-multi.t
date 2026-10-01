use Test;

# From the VERS distribution: Version::Raku adds `&[==]` as a method, while
# Version::Repology declares its own `multi infix:<==>`. The forwarder must still
# reach the builtin operator for operands the user multi does not accept.
plan 4;

class Wrapped is Version { }
BEGIN {
    Wrapped.^add_method: "==", &[==];
    Wrapped.^add_method: "cmp2", &[cmp];
}
class Other { has $.v }
multi sub infix:<==>(Other:D $a, Other:D $b --> Bool:D) { $a.v == $b.v }

ok Wrapped.new("1.0")."=="(Wrapped.new("1.0")), 'builtin == reached for equal versions';
nok Wrapped.new("1.0")."=="(Wrapped.new("2.0")), 'builtin == for different versions';
is Wrapped.new("1.0").cmp2(Wrapped.new("2.0")), Less, 'cmp forwarder';
ok Other.new(:v(1)) == Other.new(:v(1)), 'user multi still dispatches';
