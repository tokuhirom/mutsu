use Test;

# From the Archive::Tar::PP ecosystem distribution: `:8($str) // 0` on an
# empty/blank size field. A malformed body is a soft X::Str::Numeric Failure,
# not a thrown exception.

plan 8;

is :8("0000644"), 420, 'valid body still converts';

my $empty = :8("");
isa-ok $empty, Failure, ':8("") is a Failure';
is $empty.exception.^name, 'X::Str::Numeric', 'wrapping X::Str::Numeric';
is (:8("") // 0), 0, ':8("") // 0 falls back';
is (:8("12") // 0), 10, ':8("12") // 0 converts';

my $bad = :8("zz");
isa-ok $bad, Failure, ':8("zz") is a Failure';
like $bad.exception.message, /'base-8 number must begin with valid digits'/, 'message names the radix';
$bad.so;

my $s = "";
is (try { :8($s) } // 'undef'), 'undef', 'a try over the Failure yields undef';
