use Test;

plan 15;

# `$!` is scoped per routine in raku: a sub or method gets a fresh Nil on entry,
# but the CALLER's `$!` must survive the call. mutsu reset it on entry (right)
# and then merged the callee's env back over the caller's on return (wrong), so
# every call wiped the caller's error variable.

sub f() { 42 }

try { die "boom" };
f();
is $!.message, 'boom', 'a plain sub call leaves the caller $! alone';

class C { method m() { 1 } }
try { die "boom2" };
C.new.m();
is $!.message, 'boom2', 'a method call leaves the caller $! alone';

# The shape that surfaced it: reading several fields of $! in a row, where the
# first read runs a user method.
class E is Exception { has $.rc; method message { "m:$.rc" } }
try { E.new(:rc(3)).throw };
is $!.message, 'm:3', 'a user method on $! runs';
is $!.rc, 3, 'and the next field read still sees the same exception';

# A callee's own `$!` must not escape to the caller.
sub inner() { try { die "inner" }; 7 }
try { die "outer" };
inner();
is $!.message, 'outer', "a callee's own try does not export its \$! to the caller";

# A routine still gets a fresh $! on entry.
sub peek() { $!.defined ?? $!.message !! 'Nil' }
try { die "boom3" };
is peek(), 'Nil', 'a routine sees a fresh $! on entry';

# Every dispatch shape.
multi mm(Int $x) { $x }
multi mm(Str $x) { $x }
try { die "m1" };
mm(1);
is $!.message, 'm1', 'a multi call leaves the caller $! alone';

proto pp($) {*}
multi pp(Int $x) { $x }
try { die "m2" };
pp(1);
is $!.message, 'm2', 'a proto+multi call leaves the caller $! alone';

class P { method !priv() { 1 }; method pub() { self!priv() } }
try { die "m3" };
P.new.pub;
is $!.message, 'm3', 'a private method call leaves the caller $! alone';

class B { has $.v; submethod BUILD(:$!v = 5) { } }
try { die "m4" };
B.new;
is $!.message, 'm4', 'a submethod/BUILD leaves the caller $! alone';

sub fact($n) { $n <= 1 ?? 1 !! $n * fact($n - 1) }
try { die "m5" };
fact(5);
is $!.message, 'm5', 'a recursive call leaves the caller $! alone';

sub twice($x) { $x * 2 }
try { die "m6" };
3.&twice;
is $!.message, 'm6', 'a .&func call leaves the caller $! alone';

# A block, unlike a routine, SHARES its enclosing routine's $! — the merge skip
# must not apply to the closure path, or `try`/`CATCH` breaks.
try { die "boom5" };
my $b = { $!.defined ?? $!.message !! 'Nil' };
is $b(), 'boom5', 'a bare block shares the enclosing $!';

# A callee that dies is caught, and $! becomes the new error.
sub dies() { die "m8" }
try { die "m9" };
try { dies() };
is $!.message, 'm8', 'a caught error from a callee does set the caller $!';

# A successful try clears $!.
try { die "m10" };
try { 1 };
nok $!.defined, 'a successful try clears $!';
