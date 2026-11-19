use Test;

# A lazy list stops being `.is-lazy` once a pull has read it to the end of
# its source, as Rakudo's `List.is-lazy` drops `$!todo` once it is fully
# reified (#17908). An infinite generator stays lazy.

plan 16;

my $l = (lazy gather { take 1; take 2 });
ok $l.is-lazy, 'a lazy gather is lazy before any pull';
$l.raku;
nok $l.is-lazy, '... and not after .raku read it to its end';
nok $l.cache.is-lazy, '... nor is its .cache';

my $n = (lazy gather { take 1; take 2 });
my $c = $n.cache;
$c.head(5).eager;
nok $c.is-lazy, 'a cache pulled past its end is not lazy';
is $c.raku, '$(1, 2)', '... and renders without .lazy';

my $o = (1, 2).lazy;
my $oc = $o.cache;
is $oc.raku, '$(1, 2).lazy', 'a lazy list renders .lazy while it is still lazy';
nok $oc.is-lazy, '... and is not lazy once the rendering read it';

sub mk { (lazy gather { take 1; take 2 }) X (3, 4) }
my $s = mk();
$s.raku;
is $s.cache.raku, '((1, 3), (1, 4), (2, 3), (2, 4))', 'a cross over an exhausted gather renders finite';

my $z = (lazy gather { take 1; take 2 });
is $z[0], 1, 'a partial pull';
ok $z.is-lazy, '... leaves the list lazy';
is $z[5], Nil, 'a pull past the end';
nok $z.is-lazy, '... exhausts it';

my @a = lazy gather { take 1; take 2 };
ok @a.is-lazy, 'a lazy array is lazy';
@a[5];
nok @a.is-lazy, '... until an index past the end reads it whole';
is @a.elems, 2, '... after which .elems answers';

my $i = (1..Inf).map(* + 1);
$i[3];
ok $i.is-lazy, 'an infinite pipe stays lazy after a pull';
