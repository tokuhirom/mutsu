use v6.d;
use Test;

# From LLM::Prompts t/02-prompt-parsing.rakutest: `$match{*;'key'}` walks the
# named captures of the Match and then of each capture.

plan 7;

grammar G {
    rule TOP { <a> || <b> }
    token a { x }
    token b { 'y' <c> }
    token c { z }
}

my $m = G.parse('yz');
my @r = $m{*;'c'};
is @r.elems, 1, 'one capture reaches c through the wildcard level';
is @r[0].Str, 'z', 'and it is the nested Match';
ok so($m{*;'c'}), 'truthy when the nested capture exists';
is $m{*;'zz'}.elems, 1, 'a missing nested key still yields one (undefined) slot';
is-deeply $m{'b';'c'}.map(*.Str).List, ('z',), 'plain keys walk the same way';

my $n = G.parse('x');
is $n{*;'c'}.elems, 1, 'an alternative without that capture';

my %h = a => {c => 1};
is-deeply %h{*;'c'}.List, (1,), 'hash form unchanged';

done-testing;
