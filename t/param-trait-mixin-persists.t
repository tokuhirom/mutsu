use Test;

plan 7;

# A custom parameter trait whose body is `$param does SomeRole` — the shape
# every Cro::HTTP::Router parameter trait uses — has to leave the role visible
# on the parameter the signature hands back later. `Signature.params` builds a
# fresh `Parameter` on every access, so the type the trait reblessed its
# throwaway into is recorded per trait name and replayed on materialization.

role Query { }
role Header { }

multi trait_mod:<is>(Parameter:D $p, :$query! --> Nil) { $p does Query }
multi trait_mod:<is>(Parameter:D $p, :$header! --> Nil) { $p does Header }

sub search(:$term is query, :$accept is header, :$plain) { }

my @params = &search.signature.params;
is @params.elems, 3, 'the signature reports every parameter';

ok @params[0] ~~ Query, 'the trait role is visible on the parameter';
is @params[0].^name, 'Parameter+{Query}', 'and the parameter reports the mixin type';
ok @params[1] ~~ Header, 'a second trait composes its own role';
nok @params[1] ~~ Query, 'and does not leak onto the other trait';
nok @params[2] ~~ Query, 'an untraited parameter stays a plain Parameter';

# A fresh read has to agree with the first one.
ok &search.signature.params[0] ~~ Query, 'the mixin survives re-reading the signature';
