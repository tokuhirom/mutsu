use Test;

plan 2;

my @seen;
try react { whenever Supply.from-list(1) { @seen.push($_) } }
is @seen.join(','), '1', 'try react keeps whenever in the react scope';

dies-ok {
    EVAL 'whenever Supply.from-list(1) { }';
}, 'whenever outside react remains a compile-time error';
