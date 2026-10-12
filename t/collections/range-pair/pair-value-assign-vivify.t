use Test;

plan 11;

# `k => @a[i]` / `k => %h<k>` for a slot that does not exist yet captures a
# deferred element; the first `.value =` creates the slot and the Pair keeps
# reading it (#11890).
my @threads;
my $p = thread => @threads[$++];
for $p { .value = 5 }
is @threads.raku, '[5]', 'array element created by .value =';
is $p.raku, ':thread(5)', 'the Pair reads its own write (array)';
is $p.value, 5, '.value reads back';

my %h;
my $r = k => %h<x>;
is $r.value.raku, 'Any', 'unwritten hash slot reads as Any';
$r.value = 3;
is %h.raku, '{:x(3)}', 'hash entry created by .value =';
is $r.raku, ':k(3)', 'the Pair reads its own write (hash)';
is $r.gist, 'k => 3', 'gist agrees';

my @a;
my $q = k => @a[2];
$q.value = 7;
is @a.raku, '[Any, Any, 7]', 'array grown to the index';
is $q.value, 7, 'read back past the end';

# A write through the array element reaches the Pair afterwards.
$q.value = 8;
is @a[2], 8, 'second write reaches the array';
@a[2] = 9;
is $q.value, 9, 'a write to the element reaches the Pair';

done-testing;
