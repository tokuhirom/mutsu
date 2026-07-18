use v6;
use Test;

plan 8;

# A combined char class referencing a grammar token with a subtraction
# (`<+pc - [:]>`) subtracts from the WHOLE class — it must not degenerate
# into "token OR any-char-except" (RFC 3986's segment-nz-nc shape).
grammar G {
    token pc { <[:@] +alpha> }
    token nc { <+pc - [:]>+ }
    token pl { <+pc>+ }
}

nok G.parse('ab:c', rule => 'nc'), 'subtracted char stops the match';
nok G.parse('a/b', rule => 'nc'), 'chars outside the class never match';
ok G.parse('ab', rule => 'nc'), 'plain members still match';
ok G.parse('a@b', rule => 'nc'), 'non-subtracted set chars still match';
nok G.parse('a/b', rule => 'pl'), 'subtraction-free composite stays exact';
ok G.parse('a:b', rule => 'pl'), 'subtraction-free composite keeps its members';

# Nested token references fold through (unenc-pchar -> unreserved shape).
grammar H {
    token un { <[\-._~] +alpha> }
    token seg { <+un - [.]>+ }
}
ok H.parse('a-b_c', rule => 'seg'), 'nested composite class matches';
nok H.parse('a.b', rule => 'seg'), 'nested composite class honors subtraction';
