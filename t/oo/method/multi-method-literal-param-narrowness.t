use v6;
use Test;

# A literal parameter is nominally as narrow as the argument it equals, so a
# multi method with one beats a typed candidate even when its other
# parameters are untyped -- as multi-sub dispatch already ranks it
# (CSS::Writer converts `khz` to `hz` this way).

plan 5;

class W {
    proto method write-num(Numeric $, $? --> Str) {*}
    multi method write-num(1, 'em') { 'em' }
    multi method write-num($freq, 'khz') { $.write-num($freq * 1000, 'hz') }
    multi method write-num(Numeric $num, Str:D $units) { $.write-num($num) ~ $units.lc }
    multi method write-num(Numeric $num, Mu $units?) {
        my $int = $num.Int;
        ($int == $num ?? $int !! $num).Str
    }
}

is W.write-num(0.1, 'khz'), '100hz', 'literal candidate beats (Numeric, Str:D)';
is W.write-num(1, 'em'), 'em', 'all-literal candidate';
is W.write-num(2, 'em'), '2em', 'a literal that does not match falls through';
is W.write-num(42, 'Deg'), '42deg', 'typed candidate';
is W.write-num(3.5), '3.5', 'optional-unit candidate';
