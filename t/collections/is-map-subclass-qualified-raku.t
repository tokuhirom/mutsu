use Test;

# From the `immutable` distribution (ValueMap): a qualified or bound call to
# the core Map's rendering on an `is Map` subclass instance names the
# instance's own class, even when the subclass overrides `raku`.
plan 4;

my constant &map-raku = Map.^lookup('raku');
my class VM2 is Map {
    multi method raku(VM2:D:) { map-raku(self) }
}
class V3 is Map { }

is VM2.new((a => 1)).raku, 'VM2.new((:a(1)))', 'bound core raku via override';
my $x = V3.new((a => 1));
is $x.Map::raku, 'V3.new((:a(1)))', 'Map::raku names the subclass';
is $x.Map::gist, 'V3.new((a => 1))', 'Map::gist names the subclass';
is Map.^lookup('raku')($x), 'V3.new((:a(1)))', 'looked-up raku names the subclass';
