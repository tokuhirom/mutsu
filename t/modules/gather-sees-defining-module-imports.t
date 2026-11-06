use Test;
use lib 't/lib';
use GatherDefiner;

plan 6;

# A `gather` returned from a module is forced by a caller that never imported
# the module's constants; its body still resolves them where it was written.
is gd-closure()(), 5, 'a closure reads the constant its module imported';
is gd-plain()[0], 5, 'a gather forced by the caller reads it too';
is gd-eager()[0], 5, 'a gather assigned to an array in the module';
is gd-lazy()[0], 5, 'a lazy gather';
is gd-lexical()[0], 5, 'a gather over a lexical copy';
is-deeply gd-loop()[^3].List, (1, 2, 3), 'a lazy gather looping over an imported range constant';
