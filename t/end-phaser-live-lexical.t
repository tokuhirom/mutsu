use v6;
use Test;

# An END phaser is a closure over its enclosing lexical scope, so it must see
# the *final* value of every lexical it mentions -- not a copy taken when the
# phaser was registered. The captured copy only wins for a name whose declaring
# scope has since died, where it is the last surviving binding.
#
# The regression this pins: a module-scoped lexical mutated by the module's own
# subs read back at its registration-time value inside the module's END block,
# because the exit-time merge treated "captured value differs from the live one"
# as proof that the two were different variables. Rakudo's real Test.rakumod
# counts tests in exactly that shape, so its END reported "You planned N tests,
# but ran M" on any file that called lives-ok/dies-ok.

plan 9;

my $dir = $*TMPDIR.child("mutsu-end-live-{$*PID}");
$dir.mkdir;
END { try { .unlink for $dir.dir; $dir.rmdir } }

sub run-snippet($name, $source) {
    my $file = $dir.child($name);
    $file.spurt($source);
    my $proc = run($*EXECUTABLE, $file.absolute, :out, :err);
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    $out.trim
}

# 1. A module lexical mutated through the module's own subs.
my $mod = 'unit module EndLive;
my int $count;
sub bump() is export { $count = $count + 1 }
END { say "count=$count" }
';
$dir.child('EndLive.rakumod').spurt($mod);

my $use-mod = 'use lib "' ~ $dir.absolute ~ '";
use EndLive;
bump; bump; bump;
';
is run-snippet('mod.raku', $use-mod), 'count=3',
    'an END in a module sees the module lexical final value';

# 2. A mainline lexical mutated after the END was registered.
my $mainline = 'my $b = 1;
END { say "b=$b" }
$b = 9;
';
is run-snippet('main.raku', $mainline), 'b=9',
    'an END sees a mainline lexical mutated after registration';

# 3. A dead block scope still wins over a live outer variable of the same name.
my $dead = 'my $a = 1;
{ my $a = 42; END { say "a=$a" } }
';
is run-snippet('dead.raku', $dead), 'a=42',
    'an END in a dead block scope keeps that scope binding';

# 4. Registration happens once even when the enclosing sub runs repeatedly, and
#    the surviving phaser reads the shared lexical's final value.
my $twice = 'my $n = 0;
sub f { $n = $n + 1; END { say "n=$n" } }
f(); f();
';
is run-snippet('twice.raku', $twice), 'n=2',
    'an END registered inside a repeated sub runs once with the final value';

# 5. Two ENDs run last-registered-first and share the live lexical, so the
#    second one's write is visible to the first.
my $lifo = 'my $d = 1;
END { say "first d=$d" }
END { say "second d=$d"; $d = 50 }
$d = 7;
';
is run-snippet('lifo.raku', $lifo).lines.join('|'), 'second d=7|first d=50',
    'ENDs run LIFO over one shared live lexical';

# 6-9. A frame that dies still hands the phaser its final state: the captured
#      copy is the last surviving binding of a name the frame took with it, so
#      it must be the value the frame ended with, not the value at registration.
my $in-sub = 'sub f { my $x = 5; END { say "x=$x" }; $x = 7 }
f();
';
is run-snippet('in-sub.raku', $in-sub), 'x=7',
    'an END inside a sub sees that frame final lexical';

my $in-anon = 'my $b = sub { my $y = 1; END { say "y=$y" }; $y = 3 };
$b();
';
is run-snippet('in-anon.raku', $in-anon), 'y=3',
    'an END inside an anonymous sub sees that frame final lexical';

my $in-pointy = 'my &c = -> { my $z = 1; END { say "z=$z" }; $z = 4 };
c();
';
is run-snippet('in-pointy.raku', $in-pointy), 'z=4',
    'an END inside a pointy block sees that frame final lexical';

my $in-method = 'class K { method m { my $w = 1; END { say "w=$w" }; $w = 5 } }
K.m;
';
is run-snippet('in-method.raku', $in-method), 'w=5',
    'an END inside a method sees that frame final lexical';
