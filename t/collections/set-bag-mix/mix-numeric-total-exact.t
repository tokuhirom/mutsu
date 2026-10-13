use Test;

plan 6;

my $m = (a => 1/3, b => 1/6, c => 1/3).Mix;
is (+$m).raku, '<5/6>', '+$mix keeps a Rat total exact';
is (+$m).WHAT.raku, 'Rat', '+$mix of Rat weights is a Rat';
is +$m, $m.total, '+$mix agrees with .total';
is (+(a => 1, b => 2).Mix).raku, '3', 'Int weights total an Int';
is (+(a => 0.5e0, b => 1/2).Mix).raku, '1e0', 'a Num weight makes a Num total';
is ((a => 1/3, b => 1/3, c => 1/3).Mix.total).raku, '1.0', 'a Rat total of 1 stays a Rat';
