use Test;

plan 3;

# `[*;*]` on a Seq (e.g. a .map result) flattens one level like it does on an
# Array — the Seq must be treated as positional, not as a single scalar
# element. Cro::HTTP::Request's HTTP/2 cookie unpacking relies on this:
# `@headers.map({ .value.split(...).List })[*;*]`.

is ((1, 2), (3, 4)).Seq[*;*].join(","), "1,2,3,4", 'Seq[*;*] flattens';

my @a = <a b>, <c d>;
is @a.map({ $_ })[*;*].join(","), "a,b,c,d", 'map(...)[*;*] flattens';

is ((1, 2), (3, 4)).Seq[*;0].join(","), "1,3", 'Seq[*;n] selects a column';
