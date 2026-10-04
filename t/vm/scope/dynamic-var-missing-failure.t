use Test;

plan 6;

# A `$*x` nobody declared reads as an X::Dynamic::NotFound Failure inside a
# named sub, whether or not the body is a single dynamic read (#11728).
sub bare { $*nope }
sub after-stmt { 1; $*nope }
sub with-param($a) { $*nope }

isa-ok bare(), Failure, 'sole dynamic read in a named sub is a Failure';
isa-ok after-stmt(), Failure, 'dynamic read after another statement is a Failure';
isa-ok with-param(1), Failure, 'dynamic read in a sub with a parameter is a Failure';

# A declared dynamic variable still resolves.
sub reader { $*decl }
{
    my $*decl = 42;
    is reader(), 42, 'a declared dynamic variable is still found';
}
is-deeply (bare() // 'dflt'), 'dflt', 'the Failure is undefined, so // falls through';
ok bare().exception ~~ X::Dynamic::NotFound, 'the Failure wraps X::Dynamic::NotFound';
