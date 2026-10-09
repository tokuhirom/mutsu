use v6;
use Test;

# Found via SBOM::CycloneDX: `my $t := %map{$k}` for a missing key of an
# immutable Map binds Nil (a Hash binds a deferred entry that reads as Any).

plan 6;

my %m is Map = a => 1;
my $k = 'z';
my $b := %m<z>;
ok $b =:= Nil, 'is Map variable: missing literal key binds Nil';
my $c := %m{$k};
ok $c =:= Nil, 'missing computed key binds Nil';
my $mm = Map.new((a => 1));
my $d := $mm<z>;
ok $d =:= Nil, 'Map.new: missing key binds Nil';
my $e := %m<a>;
is $e, 1, 'present key still binds its value';

my %h = a => 1;
my $f := %h<z>;
nok $f =:= Nil, 'a Hash does not bind Nil';
$f = 5;
is %h<z>, 5, 'a Hash entry still autovivifies on assignment';
