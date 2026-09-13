use Test;

plan 3;

# A scoped :ignoremark subpattern is entered once for each item here. The
# subject's mark-stripped view must be shared across those entries; rebuilding
# the suffix for every item makes the parse quadratic in the document length.
grammar IgnoremarkScale {
    token TOP { [ <s> \s* ]+ }
    token s { (:ignoremark '"') ~ '"' ( <-["]>* ) }
}

my $small = ('"abcdefgh"' xx 20).join(' ');
my $large = ('"abcdefgh"' xx 640).join(' ');

ok IgnoremarkScale.parse($small), 'the small repeated document parses';
ok IgnoremarkScale.parse($large), 'the large repeated document parses';

# Average a few runs so timer granularity does not dominate the small case.
sub measure(Str:D $doc --> Numeric:D) {
    my $elapsed = 0e0;
    for ^3 {
        my $started = now;
        IgnoremarkScale.parse($doc);
        $elapsed += now - $started;
    }
    $elapsed / 3;
}

my $small-time = measure($small);
my $large-time = measure($large);
my $size-ratio = $large.chars / $small.chars;
my $time-ratio = $large-time / $small-time.max(0.000001);
ok $time-ratio < $size-ratio * 3,
    "ignoremark scaling stays near-linear (size {$size-ratio.round(0.1)}x, time {$time-ratio.round(0.1)}x)";

# vim: expandtab shiftwidth=4
