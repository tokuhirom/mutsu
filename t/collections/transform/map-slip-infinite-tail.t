use Test;

# A `.map` callback that slips a list which cannot be seen to end (an
# infinite gather or Range) streams it: the consumer reads the slipped list
# before the callback runs again, as Rakudo's map iterator does (#14295).
# mutsu kept the lazy list as one element, rendered `(...)`. Expected values
# are the `raku` (v2026.09) output.

plan 10;

sub inf { gather { my $i = 0; loop { take $i++ } } }

is-deeply (1, 2).map({ slip(inf) }).head(3).List, (0, 1, 2),
    'a deferred map over a finite list streams an infinite gather';
is-deeply (1, 2).map({ slip(1..*) }).head(5).List, (1, 2, 3, 4, 5),
    '... and an infinite Range, reading past the first callback result';
is-deeply (1..*).map({ slip(inf) }).head(3).List, (0, 1, 2),
    'a lazy map pipe streams it';
is-deeply (1..*).map(-> $a, $b { slip(inf) }).head(3).List, (0, 1, 2),
    'a multi-parameter lazy map pipe streams it';
is-deeply (1, 2, 3, 4).map(-> $a, $b { slip(inf) }).head(3).List, (0, 1, 2),
    'a multi-parameter deferred map streams it';
my @a = 1, 2;
is-deeply @a.map({ slip(inf) }).head(3).List, (0, 1, 2),
    'a map over an array streams it';
is (1, 2).map({ slip(1..*) })[3], 4, 'a subscript reads into the slipped list';

my @seen;
for (1, 2).map({ slip(1..*) }) { @seen.push($_); last if $_ > 2 }
is-deeply @seen, [1, 2, 3], 'a for loop reads the slipped list lazily';

my $s = (1, 2).map({ slip(1..*) });
is-deeply $s.head(2).List, (1, 2), 'head of a stored map Seq';

sub lz(*@v) { (gather { take $_ for @v }).lazy }
is (1, 2).map({ slip(lz($_, $_ * 10)) }).List.raku, '(1, 10, 2, 20)',
    'a lazy Slip that ends still flattens';
