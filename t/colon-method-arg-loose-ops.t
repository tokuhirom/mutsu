use Test;
plan 8;

# A leading-dot method call with colon-arg syntax must stop the argument
# before the loose word-logical operators and/or/andthen/orelse, which are
# looser than the colon-method comma list.
# `.contains: 'a' and .contains: 'b'` is `(.contains: 'a') and (.contains: 'b')`.
my $s = "LOCKING\nSUCCESS\n";

ok ($s ~~ { .contains: 'LOCKING' and .contains: 'SUCCESS' }),
    'colon-arg .contains: X and .contains: Y (both present)';
nok ($s ~~ { .contains: 'LOCKING' and .contains: 'MISSING' }),
    'colon-arg and with a missing part is false';
ok ($s ~~ { .contains: 'MISSING' or .contains: 'SUCCESS' }),
    'colon-arg .contains: X or .contains: Y';
ok ($s ~~ { not .contains: 'FAILED' and .contains: 'LOCKING' }),
    'leading not with colon-arg and';

# andthen / orelse must also terminate the colon arg.
my @seen;
sub note-it($x) { @seen.push: $x; $x }
note-it('a') andthen note-it('b');
is-deeply @seen, ['a', 'b'], 'colon-arg listop andthen terminates arg list';

# Plain method-call colon args still work (single arg, comma list).
my @a = 1, 2, 3;
is @a.join(','), '1,2,3', 'join with single colon arg';
is "a,b,c".split(',').elems, 3, 'split colon arg';

# Paren form unaffected.
ok ($s ~~ { .contains('LOCKING') and .contains('SUCCESS') }),
    'paren form .contains(X) and .contains(Y)';
