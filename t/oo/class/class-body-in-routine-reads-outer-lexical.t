use Test;

# A `my class` body inside a routine reads the program's lexicals, and a
# store from one into the routine's own lexical reaches the routine (#13675).
# Checked against rakudo.

plan 6;

my $read = "outer";

sub a { my $o; my $seen; my class A { $seen = "r=$read" }; $seen }
is a(), 'r=outer', 'a class body in a routine reads a program lexical';

sub k2 { my $o; my class P2 { $o = $read }; $o }
is k2(), 'outer', 'and stores it into the routine lexical';

sub k4 { my $o; my class P4 { $o = 7 }; $o }
is k4(), 7, 'a literal store reaches the routine lexical';

sub k5 { my $o; my $s; my class P5 { $o = $read }; $s = $o; $s }
is k5(), 'outer', 'a later by-name read sees it too';

sub nolocal { my $seen; my class N { $seen = $read }; $seen }
is nolocal(), 'outer', 'a routine with no other use of the lexical';

my $got;
my class M { $got = "m=$read" }
is $got, 'm=outer', 'a program-scope class body reads it too';
