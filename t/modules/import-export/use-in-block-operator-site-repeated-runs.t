use Test;

# An operator site remembers, for one import-set generation, that no import set
# of its block binds a user candidate for the operator (#7817). Running the
# block again re-writes its import set, so the candidates must reach the site
# every time, while a callee written outside the block never sees them.
plan 3;

use lib 't/lib';

sub outside-plus($a, $b) { try { ($a + $b).gist } // 'no candidate' }

my @r;
for 1..3 -> $i {
    {
        use UseInBlockBuiltinOpLexical;
        my $p = Pt.new(x => $i);
        my $q = Pt.new(x => 1);
        @r.push(($p + $q).gist);
        @r.push(1 + $i);
        @r.push(outside-plus($p, $q));
    }
}
is @r.join('|'), 'Pt(2)|2|no candidate|Pt(3)|3|no candidate|Pt(4)|4|no candidate',
    'the candidates reach the site on every run of the block';

my $sum = 0;
for ^50 { $sum += $_ }
is $sum, 1225, 'a built-in operator in a loop with no import in scope';

sub plain-loop { my $t = 0; for 1..10 { $t = $t * 2 + $_ }; $t }
is plain-loop(), 2036, 'and inside a routine';
