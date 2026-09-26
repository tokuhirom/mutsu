use Test;

plan 2;

# Regression from Uxmal: a negated smartmatch with a WhateverCode on the LHS
# curries the complete predicate, so it can be passed to grep.
my @promises = Promise.new, Promise.new;
is @promises.grep(*.status !~~ Kept).elems, 2,
    'WhateverCode negated smartmatch works as a grep predicate';
my &not-kept = *.status !~~ Kept;
is &not-kept(Promise.new), True,
    'the curried negated smartmatch remains callable';
