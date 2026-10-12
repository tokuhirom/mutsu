use Test;

# Every routine has its own `$!`, initially Nil; a block has none of its own
# and uses the `$!` of the scope it is written in, even when another routine
# calls it (#12902, ADR-12529 phase 3).

plan 13;

class Plain { method m { 1 } }
sub t(&c) { c() }

try { die "outer" };

sub no-params { $!.^name }
is no-params(), 'Nil', 'a sub without parameters reads its own $!';

sub positional($x) { $!.^name }
is positional(1), 'Nil', 'a sub with a positional parameter reads its own $!';

sub named(:$x) { $!.^name }
is named(:x(1)), 'Nil', 'a sub with a named parameter reads its own $!';

class Reader { method rd { $!.^name } }
is Reader.new.rd, 'Nil', 'a method reads its own $!';

my $anon = sub ($y) { $!.^name };
is $anon(1), 'Nil', 'an anonymous sub reads its own $!';

sub after-method-call($x) { Plain.new.m; $!.^name }
is after-method-call(1), 'Nil', 'a method call in the sub does not expose the caller $!';

my $seen;
t { $seen = $!.message };
is $seen, 'outer', 'a block called by a routine reads its scope $!';

t { try { die "from-block" } };
is $!.message, 'from-block', 'a try in a block called by a routine sets its scope $!';

my &later = { try { die "later" } };
try { die "again" };
t(&later);
is $!.message, 'later', 'a block made before the scope set $! again still reaches it';

sub r { t { try { die "r" } }; $!.message }
is r(), 'r', 'a try in a block passed from a sub sets the sub $!';
is $!.message, 'later', '... and not its caller one';

sub keeps($x) { try { die "inner" }; 1 }
keeps(1);
is $!.message, 'later', 'a sub that catches internally does not clobber the caller $!';

sub sets-own { try { die "own" }; $!.message }
is sets-own(), 'own', 'a try in a sub sets that sub $!';
