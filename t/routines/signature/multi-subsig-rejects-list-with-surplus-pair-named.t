use Test;

plan 6;

# A List whose elements are Pairs unpacks as a Capture whose NAMED parts are
# those Pairs. A sub-signature that names none of them and has no named slurpy
# does not match, so the next candidate takes the argument (AccessorFacade's
# `is accessor-facade(getter => &g, setter => &s)` trait).
multi choose(:$arg! where { $_.elems < 2 }) { 'short' }
multi choose(:$arg! (*@a) where { any($_.list) !~~ Int }) { 'positional-slurpy' }
multi choose(:@arg! ($first, $second, $third?)) { 'positional' }
multi choose(:@arg! (:$first!, :$second!, :$third)) { 'named' }

is choose(:arg((1, 2))), 'positional', 'positional elements take the positional sub-signature';
is choose(:arg((first => 1, second => 2))), 'named', 'Pairs as named parts skip the positional-only sub-signatures';
is choose(:arg((first => 1, second => 2, third => 3))), 'named', 'an optional named part is accepted';
is choose(:arg((1,))), 'short', 'a one-element list takes the first candidate';
is choose(:arg(("x", "y"))), 'positional-slurpy', 'non-Int positionals take the slurpy candidate';

multi plain(:$arg! (*@a)) { 'slurpy' }
multi plain(:$arg!) { 'plain' }
is plain(:arg((a => 1, b => 2))), 'plain', 'surplus named parts reject a positional-only sub-signature';
