use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang that re-spells the `sub` declarator with its own keywords reads the
# anonymous forms too (`λ ($n) { ... }`), and a slang production nested in the
# block of another one is parsed once (ADR-15762 2.8), not once per enclosing
# attempt.

plan 4;

use AnonLambdaSlang;

my $double = λ ($n) { $n * 2 };
is $double(21), 42, 'an anonymous routine with a signature';
is (lambda ( \n ) { n + 1 })(4), 5, 'the other keyword, called at once';
is (λ ( \a ) { λ ( \b ) { λ ( \c ) { λ ( \d ) { λ ( \e ) { λ ( \f ) { 7 } } } } } })(1)(2)(3)(4)(5)(6), 7,
    'six nested routines';
is λ ( &f ) { λ ( \n ) { return f(&f)(n) } }(λ ( &g ) { λ ( \n ) { n * 3 } })(5), 15,
    'the Y-combinator shape';
