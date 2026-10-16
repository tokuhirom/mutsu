use Test;
use MONKEY-TYPING;
plan 3;

# FatRatStr: `augment class Str { multi method FatRat(Str:D:) { ... } }`.
# A multi candidate whose only declared parameter is the invocant also carries
# the implicit `*%_`; that named slurpy must not make it "slurpy-only" (which
# ranks after the built-in), or the built-in `Str.FatRat` answered instead.
augment class Str {
    multi method FatRat(Str:D:) { FatRat.new(8302701, 5000000000000000000000000000000000) }
}
is '1.6605402e-27'.FatRat, FatRat.new(8302701, 5000000000000000000000000000000000),
    'invocant-only augmented multi wins over the built-in';
is '1.6605402e-27'.FatRat.WHAT.^name, 'FatRat', 'and answers a FatRat';

augment class Str {
    multi method frob-aug(Str:D:) { 'M' }
}
is 'x'.frob-aug, 'M', 'invocant-only multi with a new name';
