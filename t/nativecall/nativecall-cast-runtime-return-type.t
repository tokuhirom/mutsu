use Test;
use NativeCall;

# Both the routine's introspection and the FFI call must use the signature
# installed by nativecast, including its CPointer return type (Gnome::Glib).
plan 4;

class Handle is repr('CPointer') { }
sub dlsym(Pointer, Str --> Pointer) is native { * }
sub free(Handle) is native { * }

my $signature = :(Str --> Handle);
my $duplicate = nativecast($signature, dlsym(Pointer, 'strdup'));
is $duplicate.signature.returns, Handle, 'the cast installs the runtime signature';
is $duplicate.returns, Handle, 'Code.returns reads the installed signature';
is $duplicate.of, Handle, 'Code.of reads the same return type';
my $result = $duplicate('runtime signature');
isa-ok $result, Handle, 'the native return is boxed as the declared CPointer class';
free($result);
