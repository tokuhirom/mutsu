use v6;
use Test;

plan 7;

# An `enum` declared in a method installs its keys in the method's package, not
# in the caller's scope. The keys the method wrote into its frame must not be
# merged back over a same-named key of the caller's own enum (#16361).

class A {
    method n { enum Mode2 <Normal Other>; Normal.^name }
    method m { my enum Mode3 <Normal Other>; Normal.^name }
}
enum Volume (:Silent(-2) :Normal(0) :Verbose(1));

is Normal.^name, 'Volume', 'before the call';
is A.n, 'Mode2', 'inside the method its own enum wins';
is Normal.^name, 'Volume', 'after the call the caller keeps its own key (enum)';
is A.m, 'Mode3', 'inside the method its own `my enum` wins';
is Normal.^name, 'Volume', 'after the call the caller keeps its own key (my enum)';

# Repeated calls keep the answers stable.
A.n for ^3;
is Normal.^name, 'Volume', 'and after repeated calls';

# A plain sub's enum is a GLOBAL symbol, as before.
sub f { enum M4 <Quiet Loud>; Quiet.^name }
is f(), 'M4', 'a sub-declared enum key resolves inside the sub';
