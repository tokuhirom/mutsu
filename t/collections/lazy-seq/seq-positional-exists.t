use Test;

# `:exists` on a positional subscript of a finite Seq reads its cache (#16939).

plan 7;

my \how = [5, 77].Seq;
is how[0]:exists, True, 'index 0 exists';
is how[1]:exists, True, 'index 1 exists';
is how[2]:exists, False, 'index 2 does not exist';
is how[1], 77, 'the Seq is still readable';

my $s = (1, 2, 3).Seq;
is $s[1]:exists, True, 'in a scalar';
is (1, 2, 3).Seq[1]:exists, True, 'on a literal Seq';
is (1, 2, 3).Seq[1]:!exists, False, ':!exists';
