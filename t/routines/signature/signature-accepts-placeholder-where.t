use Test;

plan 4;

# Test::Mock uses this signature-literal form to filter captured method calls.
# Signature.ACCEPTS must bind the anonymous parameter's placeholder before
# evaluating the `where` block, just as ordinary call binding does.
class Yak { has $.shaved }
my $matcher = :($ where { !$^yak.shaved });
my $unshaved = \(Yak.new(:!shaved));
my $shaved = \(Yak.new(:shaved));

ok $unshaved ~~ $matcher, 'placeholder where accepts the matching capture';
ok $matcher.ACCEPTS($unshaved), 'ACCEPTS agrees with smartmatch';
nok $shaved ~~ $matcher, 'placeholder where rejects the non-matching capture';
nok $matcher.ACCEPTS($shaved), 'ACCEPTS rejects the non-matching capture';
