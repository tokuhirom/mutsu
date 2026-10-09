use v6;
use Test;

# Found via SBOM::CycloneDX: `bless` initialized a private `has @!t` from a
# same-named named argument (and type-checked it), which BUILDALL does not do.

plan 3;

class LT { has $.n }
class T { has $.n }
class A {
    has LT @!t;
    has T $!t;
    submethod TWEAK(:$t) { @!t = $t.map({ LT.new(n => $_) }) if $t ~~ Positional }
    method t { @!t.List }
}
is A.new(t => <a b>).t.map(*.n).join, 'ab', 'new';
my %h = t => <a b>;
is A.bless(|%h).t.map(*.n).join, 'ab', 'bless with a flattened hash';
is A.bless(t => <a b>).t.map(*.n).join, 'ab', 'bless with a named arg';
