use Test;

plan *;

my $pulls = 0;
my $fresh = (1, 2).map({ $pulls++; $_ });
ok !$fresh.is-lazy, 'finite map Seq is not lazy';
is $pulls, 0, 'querying laziness does not pull its source';
my $cached = $fresh.cache;
isa-ok $cached, List, 'cache returns a List view';
is-deeply $cached.List, (1, 2), 'cached view reads all elements';
is-deeply $cached.List, (1, 2), 'cached view remains repeatable';
is $pulls, 2, 'cache pulls each element only once';

my $consumed = (3, 4).map(* + 1);
$consumed.iterator;
throws-like { $consumed.is-lazy }, X::Seq::Consumed,
    'querying a consumed Seq refuses without consuming again';

my $path = $*TMPDIR.add("seq-state-row-guards-{$*PID}.txt");
$path.spurt("alpha beta\ngamma delta\n");
LEAVE try $path.unlink;

for <lines words> -> $method {
    my $handle = $path.open(:r);
    my $source = $handle."$method"();
    my $indexed = $source.kv;
    is $handle.tell, 0, 'lines.kv leaves the IO source unpulled' if $method eq 'lines';
    throws-like { $source.List }, X::Seq::Consumed,
        "$method.kv consumes the original Seq";
    my @expected = $method eq 'lines'
        ?? (0, 'alpha beta', 1, 'gamma delta')
        !! (0, 'alpha', 1, 'beta', 2, 'gamma', 3, 'delta');
    is-deeply $indexed.List, @expected.List,
        "$method.kv streams the indexed elements";
    $handle.close;
}

done-testing;
