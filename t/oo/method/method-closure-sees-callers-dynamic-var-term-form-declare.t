use lib 't/lib';
use Test;

# A method added through the HOW (`type.^add_multi_method: $name, method (T:U:) { ... }`)
# reads `$*FLAG` from the frame that CALLS it. The snapshot of the declaring
# scope the method keeps must not shadow that binding -- it did when the class
# was declared as a term (`given dynmodel { } { ... }`) in a file that also
# declares `my $*FLAG` at file scope and a later `dynmodel` statement.

use DeclareDynamicMethod;

plan 5;

given dynmodel { } {
    my $*FLAG = True;
    is .a, "read-a", "term-form class: the method sees the block's dynamic variable";
}

my $*FLAG = True;

dynmodel Later { }

is Later.a, "read-a", "statement-form class";

sub call-it(\type) { my $*FLAG = True; type.a }
is call-it(Later), "read-a", "a dynamic variable bound in a calling sub";

my \term = dynmodel Term { };
{
    my $*FLAG = True;
    is term.a, "read-a", "term form bound to a variable";
}

{
    my $*FLAG = Nil;
    dies-ok { Later.a }, "the caller's own falsy binding still reaches the method";
}
