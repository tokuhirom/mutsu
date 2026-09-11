use v6;
use Test;

# A Str enum's value is a Str in Pair.raku's key position. The enum itself
# keeps its qualified `.raku`, but the Pair uses the string value for the
# colonpair form, just as a Hash does.

plan 3;

our Str enum Sel « :Alpha<a-val> »;

is (Sel::Alpha => 'x').raku, ':a-val("x")',
    'a Str-enum Pair key uses its string value';
is Pair.new(Sel::Alpha, 'x').raku, ':a-val("x")',
    'Pair.new preserves the Str-enum key rendering';
is %(Sel::Alpha => 'x').raku, '{:a-val("x")}',
    'a Hash with a Str-enum key keeps its existing rendering';

done-testing;
