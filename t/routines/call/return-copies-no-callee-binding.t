use Test;

# A call's return copies none of the callee's bindings back into the caller
# (ADR-12529 §3, slice 41): what the callee declares stays its own, and what
# it writes in its caller's scope arrives through the variable itself (a
# shared cell, the routine's `$/` / `$!`, or a carried runtime-named write).
# Checked against rakudo.

plan 8;

sub nested-dyn() { if True { my $*leak = 1 }; 0 }
nested-dyn();
nok (try $*leak).defined, 'a dynamic declared in a nested block stays there';

sub eval-dyn() { EVAL 'my $*z = 5'; 1 }
eval-dyn();
nok (try $*z).defined, 'a dynamic an EVAL declares stays there';

my &b = { "xy" ~~ /y/ };
b();
is ~$/, 'y', 'a block called by name sets its routine\'s $/';

my &c = { try die "boom" };
c();
is $!.message, 'boom', 'and its $!';

sub own-match() { my &bb = { "ab" ~~ /b/ }; bb(); ~$/ }
is own-match(), 'b', 'inside a sub, the sub\'s $/';

sub cb-user(&f) { f() }
"q" ~~ /q/;
cb-user({ "zz" ~~ /z/ });
is ~$/, 'z', 'a block passed to a sub sets its own routine\'s $/';

my $count = 0;
sub bump() { $count++ }
bump() for ^3;
is $count, 3, 'an outer lexical written by a sub';

sub rebind() { $*t := $*root }
sub r() { my $*root = 'root'; my $*t = 'orig'; rebind(); $*t }
is r(), 'root', 'a caller-declared dynamic rebound by a callee';
