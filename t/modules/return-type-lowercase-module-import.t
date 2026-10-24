use lib 't/lib';
use Test;
use lowercasepkg::Use;

plan 2;

# `--> Fact` resolves to `lowercasepkg::Types::Fact`; a package-qualified type
# with a lower-case first segment is a type, not a value the routine returns.
my $r = ev(3);
isa-ok $r, lowercasepkg::Types::Fact, 'routine returning an imported type object from a lower-case module';
is $r.r, 'ok', 'the explicit return value is kept';
