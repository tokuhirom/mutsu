use Test;

# From the PDF distribution (PDF::COS.coerce): a named `is rw` multi candidate
# is not applicable to a literal, so the sibling `is copy` candidate runs.
plan 5;

class C {
    multi method coerce(Bool :$bool! is rw) { "rw" }
    multi method coerce(Bool :$bool! is copy) { "copy" }
}
my $v = True;
is C.coerce(:bool(True)), "copy", 'literal named arg skips the rw candidate';
is C.coerce(:bool($v)), "rw", 'variable named arg takes the rw candidate';

multi sub g(Int :$x! is rw) { "rw" }
multi sub g(Int :$x!) { "plain" }
my $j = 4;
is g(:x(5)), "plain", 'sub: literal';
is g(:x($j)), "rw", 'sub: variable';

multi sub h(Int :$x! is rw) { $x++; "rw" }
multi sub h(Int :$x! is copy) { h(:$x) }
is h(:x(1)), "rw", 'copy candidate re-dispatches to rw with a variable';
