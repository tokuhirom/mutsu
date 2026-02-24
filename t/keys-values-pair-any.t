use Test;

plan 10;

my %h = a => 1, b => 2;
is +%h, 2, '+%hash uses hash element count';

my $pair = a => 42;
is ~$pair.keys, "a", '$pair.keys works';
is ~keys($pair), "a", 'keys($pair) works';
is ~$pair.values, 42, '$pair.values works';
is ~values($pair), 42, 'values($pair) works';

my $x;
lives-ok { $x.keys }, 'Any.keys is callable';
lives-ok { $x.values }, 'Any.values is callable';

is ((Mu) => 4).keys.raku, (Mu,).Seq.raku, 'parenthesized Mu key is not auto-quoted';
is ((Mu) => Mu).kv.raku, (Mu, Mu).Seq.raku, 'pair kv preserves Mu key/value';
is (4 => Mu).values.raku, (Mu,).Seq.raku, 'pair values preserves Mu value';
