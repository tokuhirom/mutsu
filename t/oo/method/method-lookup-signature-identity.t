use Test;

plan 3;

class C { }

# A native method's synthesized Signature is built once, so repeated lookups
# do not pile new Signature values into the process-global registry (#13009).
ok C.^lookup('new').signature === C.^lookup('new').signature, 'repeated lookups share one Signature';
ok Str.^find_method('chars').signature === Str.^find_method('chars').signature, 'same for find_method';

my $before = C.^lookup('new').signature;
for ^2000 { my $s = C.^lookup('new').signature }
ok $before === C.^lookup('new').signature, 'identity holds after many lookups';
