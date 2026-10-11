use Test;

# From Arithmetic::PaperAndPencil (t/07-mult): a class-scoped `sub` reads and
# writes the class body's `my @a` / `my %h`, never a same-named array or hash
# of the calling frame (the env is flattened, so the caller's binding used to
# win for `@`/`%` names).

plan 12;

class Z {
    my @arr = <a b>;
    my %h = a => 1;
    our sub add($x)  { @arr.push($x); 1 }
    our sub hadd($x) { %h{$x} = 1; 1 }
    our sub reset()  { @arr = <q>; %h = (); }
    our sub show()   { @arr.join ~ %h.keys.sort.join }
    our sub len(Int $r) { @arr.elems + $r }
    method m() { @arr.join }
}

my @arr = 9;
my %h = z => 1;

is Z::show, 'aba', 'sub reads its own static array and hash';
is Z::len(10), 12, 'sub with a positional parameter reads its own static';
is Z.m, 'ab', 'method reads its own static';

Z::add('c');
is Z::show, 'abca', 'push inside the sub lands in the static array';
is @arr.join, '9', "caller's array untouched by the push";

Z::hadd('c');
is Z::show, 'abcac', 'hash element store lands in the static hash';
is %h.keys.join, 'z', "caller's hash untouched";

Z::reset;
is Z::show, 'q', 'whole-container assignment lands in the statics';
is @arr.join, '9', "caller's array untouched by the assignment";
is %h.keys.join, 'z', "caller's hash untouched by the assignment";

sub outer() { my @arr = 1, 2; Z::show }
is outer(), 'q', 'a caller-frame lexical of the same name does not shadow the static';

sub param(@arr) { Z::show }
is param([7, 8]), 'q', 'a caller parameter of the same name does not shadow it either';
