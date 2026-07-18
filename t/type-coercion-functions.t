use Test;

plan 6;

# Function-form type coercions delegate to the method form
# (JSON::Unmarshal's `multi _unmarshal(Any:D $json, Rat) { Rat($json) }`).
is Rat(4.2), 4.2, 'Rat(Num-ish)';
isa-ok Rat("3.5"), Rat, 'Rat(Str) parses';
is Rat("3.5"), 3.5, '... to the right value';
isa-ok FatRat(0.5), FatRat, 'FatRat() coerces';
is Complex("1+2i"), Complex.new(1, 2), 'Complex(Str) parses the complex string';
is "1+2i".Complex, Complex.new(1, 2), '.Complex on Str parses too';

done-testing;
