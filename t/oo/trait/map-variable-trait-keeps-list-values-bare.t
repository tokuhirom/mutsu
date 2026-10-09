use v6;
use Test;

# Found via SBOM::CycloneDX: a `my %m is Map` holds its values bare, unlike a
# Hash, whose element store itemizes a list.

plan 4;

my %m is Map = a => "x", r => <p q>;
is %m.raku, 'Map.new((:a("x"),:r(("p", "q"))))', 'Map var .raku shows the list bare';
is %m<r>.raku, '("p", "q")', 'element is not itemized';

my %h = a => "x", r => <p q>;
is %h.raku, '{:a("x"), :r($("p", "q"))}', 'control: Hash itemizes';

sub f(\args) { args.raku }
is f(%m), 'Map.new((:a("x"),:r(("p", "q"))))', 'through a raw parameter';
