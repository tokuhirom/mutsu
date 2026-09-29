use Test;

plan 10;

# Baggy.hash / Mixy.hash are parameterized Hashes (Type/Baggy.rakudoc).
for <hash Hash> -> $m {
    is bag(<a b b>)."$m"().^name, 'Hash[UInt,Mu,Any]', "Bag.$m type name";
}
is bag(<a b b>).hash.keyof.^name, 'Mu', 'Bag.hash.keyof';
is bag(<a b b>).hash.of.^name, 'UInt', 'Bag.hash.of';
is mix(<a b b>).hash.^name, 'Hash[Real,Mu,Any]', 'Mix.hash type name';
is mix(<a b b>).hash.of.^name, 'Real', 'Mix.hash.of';
is-deeply bag(<a b b>).hash.sort.List, (a => 1, b => 2), 'Bag.hash contents';
is set(<a b>).hash.^name, 'Hash', 'Set.hash stays a plain Hash';
is (my %h = bag(<a b b>).hash).^name, 'Hash', 'assigning it gives a plain Hash';
is-deeply %h<b>, 2, 'values survive';
