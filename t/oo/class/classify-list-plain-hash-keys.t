use Test;

plan 8;

my %h;
%h.classify-list({ $_ % 2 }, 1..6);
is-deeply %h.keys.sort.map({ .WHAT }).list, (Str, Str), 'classify-list into a plain hash stringifies its keys';
is %h<0>.join(','), '2,4,6', 'the even bucket';
is %h<1>.join(','), '1,3,5', 'the odd bucket';
is %h.keyof.raku, 'Str(Any)', 'the hash keeps its plain key type';

my %g;
%g.categorize-list({ ($_ % 2, 5) }, 1..3);
is-deeply %g.keys.sort.list, ('0', '1', '5'), 'categorize-list into a plain hash stringifies its keys';
is %g<5>.join(','), '1,2,3', 'the shared bucket';

my %o{Any};
%o.classify-list({ $_ % 2 }, 1..4);
is-deeply %o.keys.sort.map({ .WHAT }).list, (Int, Int), 'an object-hash target keeps its key objects';

my %r = (1..6).classify({ $_ % 2 });
is %r.keys.sort.join(','), '0,1', 'standalone classify assigned to a plain hash';
