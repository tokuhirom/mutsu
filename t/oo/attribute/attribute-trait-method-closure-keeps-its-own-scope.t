use lib 't/lib';
use Test;

# An attribute trait that installs a method literal through the MOP
# (`$attr.package.^add_multi_method: $name, my method (...) { ... }`) hands the
# class a closure over ITS creator's lexicals (`$attr`, `$name`). When the
# declaring file also has a variable the closure reads from its own frame
# (here the dynamic `$*FLAG`, declared before the class), the class
# declaration's own scope capture replaced the closure's environment and the
# closure saw `$attr` and `$name` as Nil.

use AttrTraitMethodClosure;

plan 6;

my $*FLAG = True;

class Plain {
    has $.x is reader;
}
is Plain.x, '$!x/x/Plain', "the closure reads its creator's `\$attr` and `\$name`";

class Twice {
    has $.alpha is reader;
    has $.beta  is reader;
}
is Twice.alpha, '$!alpha/alpha/Twice', "first attribute of a class with two readers";
is Twice.beta, '$!beta/beta/Twice', "second attribute: each closure keeps its own scope";

{
    my $*FLAG = True;
    class Inner {
        has $.y is reader;
    }
    is Inner.y, '$!y/y/Inner', "a class declared in a block";
}

sub make-class { my class InSub { has $.z is reader }; InSub }
is make-class().z, '$!z/z/InSub', "a class declared in a sub body";

dies-ok { my $*FLAG = Nil; Plain.x }, "the caller's falsy `\$*FLAG` still reaches the closure";
