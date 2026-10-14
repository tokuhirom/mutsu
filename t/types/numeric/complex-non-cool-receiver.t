use Test;

plan 13;

# `.Complex` is declared on Cool: a receiver that is not a Cool has no such
# method, and a numeric type object cannot be coerced (#13116).
sub err-name(&code) { try code(); $!.^name }

is err-name({ Blob.Complex }), 'X::Method::NotFound', 'Blob type object';
is err-name({ Int.Complex }), 'X::Parameter::InvalidConcreteness', 'Int type object';
is err-name({ Rat.Complex }), 'X::Parameter::InvalidConcreteness', 'Rat type object';
is err-name({ Num.Complex }), 'X::AdHoc', 'Num type object cannot unbox';
is err-name({ (-> {}).Complex }), 'X::Method::NotFound', 'a Block';
is err-name({ Promise.new.Complex }), 'X::Method::NotFound', 'a Promise';
is err-name({ (a => 1).Complex }), 'X::Method::NotFound', 'a Pair';
is err-name({ set(1).Complex }), 'X::Method::NotFound', 'a Set';
is err-name({ (1..*).lazy.Complex }), 'X::Cannot::Lazy', 'a lazy list';

# What stays as it was.
is "3".Complex, <3+0i>, 'a Str parses';
is (1, 2).Complex, <2+0i>, 'a List is its element count';
is-deeply Complex.Complex, Complex, 'Complex.Complex is the type object';
is 5.Complex, <5+0i>, 'an Int';
