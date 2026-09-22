use Test;

plan 1;

# Map::Leaflet uses two bracket-destructuring parameters in one multi method.
class PairBuckets {
    multi method join([ $a, $b ], [ $c, $d ], *%options) {
        "$a/$b/$c/$d/{%options<mode>}";
    }
}

is PairBuckets.new.join([1, 2], [3, 4], :mode<fast>),
    '1/2/3/4/fast',
    'multiple bracket-destructuring parameters do not redeclare anonymous @';
