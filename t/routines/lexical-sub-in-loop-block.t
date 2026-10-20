use Test;

# A loop body's block that declares a sub re-installs it on every iteration,
# and the routine lookup refills that name's key list each time. The refill
# reads the qualified-name tail index plus the unqualified keys recorded for
# the name, not the whole registry (#9170). Whichever way it is found, each
# iteration calls its own block's sub, and the outer sub of the same name is
# what the code after the block calls.

plan 6;

sub g { 'outer' }

my @seen;
for ^3 -> $i {
    { sub g { "block $i" }; @seen.push: g() }
    @seen.push: g();
}
is @seen.join(','), 'block 0,outer,block 1,outer,block 2,outer',
    'each iteration calls its block sub, then the outer one';

my @two;
for ^2 {
    { sub h { 'first' }; @two.push: h() }
    { sub h { 'second' }; @two.push: h() }
}
is @two.join(','), 'first,second,first,second', 'sibling blocks each see their own sub';

my $n = 0;
for ^200 { { sub k { 1 }; $n += k() } }
is $n, 200, 'many iterations add up';

# A routine registered after the loop is found, and one only a block declared
# is not visible outside it.
for ^2 { { sub only-in-block { 1 }; only-in-block() } }
sub after-loop { 'after' }
is after-loop(), 'after', 'a routine declared after the loop is found';
throws-like { EVAL 'only-in-block()' }, Exception, 'a block sub is not visible after its block';

my @multi;
for ^2 { { multi m(Int) { 'int' }; multi m(Str) { 'str' }; @multi.push: m(1), m('a') } }
is @multi.join(','), 'int,str,int,str', 'a block multi resolves on every iteration';
