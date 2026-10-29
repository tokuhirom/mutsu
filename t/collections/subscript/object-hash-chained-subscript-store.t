use Test;

plan 12;

# A chained store through an object-hash key (`%h{$obj}[3] = v`) finds the
# entry `%h{$obj}` does, instead of filing a second one under the key's string
# form (CSS::Properties' per-module property cache never hit because of it).
class M { }
my $m = M.new;

my %a{M}; %a{$m} = []; %a{$m}[3] = 1;
is %a.elems, 1, 'assign: one entry';
is %a{$m}.raku, '$[Any, Any, Any, 1]', 'assign: the stored array';
ok %a.keys[0] === $m, 'assign: the key is still the object';

my %b{M}; %b{$m} = []; %b{$m}[3] //= 1;
is %b.elems, 1, '//=: one entry';
my %c{M}; %c{$m} = []; %c{$m}[3] ||= 1;
is %c.elems, 1, '||=: one entry';
my %d{M}; %d{$m} = []; %d{$m}[3]++;
is %d.elems, 1, '++: one entry';
my %e{M}; %e{$m} = []; %e{$m}[3] += 1;
is %e.elems, 1, '+=: one entry';
my %f{M}; %f{$m} = {}; %f{$m}<k> //= 1;
is %f.elems, 1, 'hash value //=: one entry';

my %g{M}; %g{$m}[2] //= 5;
is %g{$m}.raku, '$[Any, Any, 5]', 'autovivification through the object key';
is %g.elems, 1, 'autovivification: one entry';

my %h{M}; %h{$m}<k> = 1; %h{$m}<j> = 2;
is %h{$m}.raku, '${:j(2), :k(1)}', 'two chained stores share the entry';

# a cache keyed by an object, filled lazily (the real-world shape)
my %cache{M};
sub slot($o, $n) { %cache{$o}[$n] //= M.new }
ok slot($m, 2) === slot($m, 2), 'a lazily filled per-object cache hits';
