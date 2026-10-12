use Test;

plan 5;

# A trailing BEGIN block runs at BEGIN time but is still the statement whose
# value the EVAL'd unit has.
is EVAL('5; BEGIN { 1 }'), 1, 'BEGIN after a statement';
is EVAL('BEGIN { 4 }'), 4, 'a unit of one BEGIN';
is EVAL('5; BEGIN { 2 }; 3'), 3, 'a later expression still wins';
is EVAL('my $x = 2; BEGIN { 7 }'), 7, 'BEGIN after a declaration';
is EVAL('5; (BEGIN { 1 })'), 1, 'the expression form is unchanged';
