use Test;

plan 5;

my $proto = do { my proto qux(Numeric) {*} };
my $parenthesized = (my proto zip(Numeric) {*});
isa-ok $proto, Sub, 'a block-final my proto yields its dispatcher';
is $proto.signature.gist, $parenthesized.signature.gist,
    'the yielded proto keeps the same signature as expression-position proto';

sub returned-proto { my proto baz(Str) {*} }
my $parenthesized-str = (my proto zot(Str) {*});
isa-ok returned-proto(), Sub, 'a routine can return its final proto declaration';
is returned-proto().signature.gist, $parenthesized-str.signature.gist,
    'the returned proto keeps the same signature as expression-position proto';

is $proto.WHAT, $parenthesized.WHAT,
    'block-final and parenthesized proto declarations yield the same type';
