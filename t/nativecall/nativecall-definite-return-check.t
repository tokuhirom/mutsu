use Test;
use NativeCall;

# rakudo 2026.11's NativeCall maps a definiteness-constrained return type
# (`--> Str:D`) to its base type for the native call and checks the value it
# returns: a NULL `char *` is an undefined Str, which `Str:D` rejects (#19210).
# With 2026.06 the type object came back unchecked.

plan 5;

sub getenv(Str --> Str:D) is native {*}
sub getenv-any(Str --> Str) is native is symbol('getenv') {*}

# PATH is in the process environment the C library reads (an assignment to
# %*ENV is not, on rakudo).
is getenv('PATH'), %*ENV<PATH>, 'a defined result passes the --> Str:D check';

throws-like { getenv('MUTSU_NC_DEFINITE_RETURN_UNSET_XYZ') },
    X::TypeCheck::Return,
    message => 'Type check failed for return value; expected Str:D but got Str (Str)',
    'a NULL result fails the --> Str:D check';

nok getenv-any('MUTSU_NC_DEFINITE_RETURN_UNSET_XYZ').defined,
    'an unconstrained --> Str still returns the type object';

sub abs-d(int32 --> int32) is native is symbol('abs') {*}
is abs-d(-7), 7, 'a native int return is untouched';

sub c-abs-defined(int32 --> Int:D) is native is symbol('abs') {*}
is c-abs-defined(-3), 3, '--> Int:D maps to the Int return code';
