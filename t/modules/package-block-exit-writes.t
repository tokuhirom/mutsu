use Test;

# A `package`/`module`/`class` block runs over a block tier of its own, and
# its exit reads back only what the body wrote by name (#9171). What leaves
# the block, and what stays inside it, is unchanged.

plan 10;

my $x = 1;
my $top = 'top';
my @a = 1, 2;
module M {
    $x = 2;
    our $top = 'our';
    my $priv = 'p';
    our sub get { $priv }
    @a.push(3);
}
is $x, 2, 'a write to an enclosing lexical propagates out';
is $top, 'top', 'an `our` declaration does not overwrite the outer lexical';
is $M::top, 'our', 'the `our` variable is the package one';
is M::get(), 'p', 'a sub sees the block lexical after the block exits';
is-deeply @a, [1, 2, 3], 'a mutated enclosing array';

package P { my $cfg = 10; our sub v { $cfg + 1 } }
is P::v(), 11, 'a package block lexical captured by an `our` sub';

class K { my $count = 0; method inc { ++$count } }
K.inc;
is K.inc, 2, 'a class block lexical shared by method calls';

# Package blocks in a row, each with its own lexical of the same name.
package Q0 { my $v = 0; our sub v { $v } }
package Q1 { my $v = 1; our sub v { $v } }
package Q2 { my $v = 2; our sub v { $v } }
is-deeply (Q0::v(), Q1::v(), Q2::v()), (0, 1, 2), 'blocks keep separate lexicals';

my $leak = (try EVAL q[$priv]) // 'not visible';
is $leak, 'not visible', 'a block lexical does not leak into the enclosing scope';

dies-ok { EVAL q[module Boom { die 'boom' }] }, 'an error in the block propagates';
