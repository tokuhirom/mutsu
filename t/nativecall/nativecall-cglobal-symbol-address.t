use Test;
use NativeCall;

# Gnome::Glib casts cglobal(..., Pointer) to a runtime Signature. Reading
# machine code as a pointer here used to crash when the cast was called.
plan 6;

sub dlsym(Pointer, Str --> Pointer) is native { * }
sub getpid(--> int32) is native { * }

my $function = cglobal(Str, 'getpid', Pointer);
is $function.Int, dlsym(Pointer, 'getpid').Int,
    'a Pointer target returns the function symbol address';
my $call = nativecast(:(--> int32), $function);
is $call(), getpid(), 'the symbol can be cast to a callable signature';

# Data symbols obey the same rule: no function/data classification is needed.
my $data = cglobal(Str, 'optind', Pointer);
is $data.Int, dlsym(Pointer, 'optind').Int,
    'a Pointer target returns the data symbol address';
is nativecast(Pointer[int32], $data).deref,
    cglobal(Str, 'optind', int32), 'explicit dereference reads the scalar value';

my $typed = cglobal(Str, 'optind', Pointer[int32]);
is $typed.Int, $data.Int, 'a parameterized Pointer also wraps the symbol address';
is $typed.deref, cglobal(Str, 'optind', int32),
    'a parameterized Pointer dereferences the declared element type';
