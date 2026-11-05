use Test;

# `.new` of a class deriving from a built-in container (Hash, Map, Array, List,
# Set, Bag, Mix, SetHash, BagHash) builds the object without `bless`, so Rakudo
# never runs its BUILD/TWEAK submethods; only an explicit `self.bless` does
# (found via PDF's `is Hash` classes, #15656).
plan 10;

my @ran;
class CH is Hash {
    submethod TWEAK { @ran.push: "Hash TWEAK" }
    submethod BUILD { @ran.push: "Hash BUILD" }
}
class CM is Map { submethod TWEAK { @ran.push: "Map TWEAK" } }
class CA is Array { submethod TWEAK { @ran.push: "Array TWEAK" } }
class CL is List { submethod TWEAK { @ran.push: "List TWEAK" } }
class CS is Set { submethod TWEAK { @ran.push: "Set TWEAK" } }
class CB is Bag { submethod TWEAK { @ran.push: "Bag TWEAK" } }
class CX is Mix { submethod TWEAK { @ran.push: "Mix TWEAK" } }
class CSH is SetHash { submethod TWEAK { @ran.push: "SetHash TWEAK" } }
class CBH is BagHash { submethod TWEAK { @ran.push: "BagHash TWEAK" } }

.new for CH, CM, CA, CL, CS, CB, CX, CSH, CBH;
is @ran.elems, 0, 'no BUILD/TWEAK ran for any built-in container subclass';

lives-ok { CM.new((:a(1))) }, 'Map subclass accepts a positional pair';
lives-ok { CA.new(1, 2) }, 'Array subclass accepts positionals';
is CA.new(1, 2).elems, 2, 'and keeps them';

class P { submethod TWEAK { @ran.push: "plain TWEAK" } }
P.new;
is @ran.join(","), "plain TWEAK", 'a plain class still runs TWEAK';

class Dict is Hash {
    method new(*%a) { self.bless(|%a) }
    submethod TWEAK(:$dict!) { self{.key} = .value for $dict.pairs.sort }
}
my $d = Dict.new(:dict(%(:a<x>, :b<y>)));
is $d<a>, 'x', 'an explicit bless runs TWEAK';
is $d.keys.sort.join(","), 'a,b', 'and TWEAK can store entries';

class D3 is Hash {
    method new(*%a) { self.bless(|%a) }
    submethod TWEAK { self.BIND-KEY("q", 7); self.ASSIGN-KEY("s", 9) }
}
my $f = D3.new;
is $f<q>, 7, 'BIND-KEY inside TWEAK of a blessed Hash subclass';
is $f<s>, 9, 'ASSIGN-KEY inside TWEAK of a blessed Hash subclass';

class D2 is Hash { method put($k, $v) { self.BIND-KEY($k, $v) } }
my $e = D2.new; $e.put("z", 5);
is $e<z>, 5, 'BIND-KEY on a built Hash subclass';
