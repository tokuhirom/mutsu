use Test;
use NativeCall;

# NativeCall's five helper routines are Rakudo module exports, not Raku
# builtins -- `Language/perl-func.rakudoc` lists none of them, and mutsu's
# working agreement is that a routine belongs in the builtin set only if it does.
# They used to be dispatched as unconditional builtins here, which had two
# observable consequences: they were visible without importing the module, and
# there was no `&nativecast` to take, pass or wrap.

plan 15;

for <cglobal nativecast nativesizeof explicitly-manage refresh> -> $name {
    ok defined(::("\&$name")), "&$name is a real routine ($name)";
}

# First-class: takeable, bindable and callable through the `&` form. A
# distribution that passes `&nativecast` around, or that looks it up as
# `::('&nativecast')`, now finds it.
my &cast = &nativecast;
my &sizeof = &nativesizeof;

sub c_getenv(Str --> Pointer) is native('c') is symbol('getenv') { * }
my $path = c_getenv('PATH');

ok cast(Str, $path).chars > 0, 'a bound &nativecast casts a Pointer to Str';
is sizeof(int64), 8, 'a bound &nativesizeof reports a C width';
is (&nativesizeof)(uint8), 1, 'and the parenthesised & form calls too';

# The arguments a NativeCall helper takes are not ordinary values -- a type
# object, a parameterized type, and a Signature literal -- so they have to
# survive being bound to a Raku parameter rather than reaching a builtin raw.
is nativesizeof(Pointer), 8, 'a type object binds as an argument';
isa-ok nativecast(CArray[uint8], $path), CArray, 'a parameterized type binds';
ok nativecast(Str, $path) ~~ Str, 'and the cast result is the target type';

sub c_dlopen(Str, int32 --> Pointer) is native('c') is symbol('dlopen') { * }
sub c_dlsym(Pointer, Str --> Pointer) is native('c') is symbol('dlsym') { * }
my $sqrt = nativecast(:(num64 --> num64), c_dlsym(c_dlopen('libm.so.6', 2), 'sqrt'));
is $sqrt(16e0), 4e0, 'a Signature literal binds, and the cast pointer is callable';

# `map`/`grep` over them: the plainest proof they are ordinary routines.
is-deeply (int8, int16, int32, int64).map(&nativesizeof).List, (1, 2, 4, 8),
    '&nativesizeof works as a map body';

# Being an import rather than an ambient builtin is the point: a program that
# does not `use NativeCall` must not see them. Rakudo answers "Undeclared
# routine" at compile time; mutsu's undeclared-routine check does the same.
my $proc = run($*EXECUTABLE, '-e', 'say nativesizeof(int64)', :out, :err);
$proc.out.slurp(:close);
my $err = $proc.err.slurp(:close);
ok $err.contains('nativesizeof'), 'calling nativesizeof without the module names it in the error';
nok $proc.so, 'and fails rather than answering 8';
