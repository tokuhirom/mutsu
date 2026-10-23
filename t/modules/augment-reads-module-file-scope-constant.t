use lib 't/lib';
use Test;

plan 3;

# A method an `augment` installs reads the file-scope `my constant`s of the
# module file it is written in, also when it is called after the load.
use AugmentConstReader;

is-deeply AugmentConstReader.list, ('a', 'b'), 'a positional constant';
is AugmentConstReader.scalar, 5, 'a scalar constant';
is AugmentConstReader.hash-a, 1, 'a hash constant';
