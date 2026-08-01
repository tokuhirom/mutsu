use v6;
use Test;

# The sub form of grep returns a Seq, like the method form — a
# `--> Seq` return constraint on a sub ending in a grep call must pass
# (Base64's encoder is `grep *.so, ... --> Seq`).

plan 3;

is (grep *.so, (1, 2)).^name, 'Seq', 'sub-form grep returns a Seq';
is (1, 2).grep(*.so).^name, 'Seq', 'method-form grep returns a Seq';

sub seqy(--> Seq) { grep * > 1, (1, 2, 3) }
is-deeply seqy.List, (2, 3), 'a --> Seq sub ending in sub-form grep passes its return check';
