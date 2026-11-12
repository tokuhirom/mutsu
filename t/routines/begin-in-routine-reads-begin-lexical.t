use Test;

# A BEGIN inside a routine runs at compile time even when it reads a unit
# lexical declared by a blockless `BEGIN my` (#17567). Checked against rakudo.

plan 6;

BEGIN my @log;

BEGIN my $u = 1;
sub f { BEGIN @log.push("sub $u") }

class C { method m { BEGIN @log.push("method $u") } }

BEGIN my ($p, $q) = 5, 6;
sub k { BEGIN @log.push("pq {$p + $q}") }

BEGIN my @a = 1, 2;
sub g { BEGIN @a.push(3) }

BEGIN my $n = 10;
sub h { BEGIN $n++ }

is @log[0], 'sub 1', 'a BEGIN in an uncalled sub runs and reads the BEGIN lexical';
is @log[1], 'method 1', 'so does one in a method';
is @log[2], 'pq 11', 'a lexical of a blockless BEGIN list declaration is reached';
is @log.elems, 3, 'each BEGIN ran once';
is-deeply @a, [1, 2, 3], 'a BEGIN in a routine changes a BEGIN array';
is $n, 11, 'and a BEGIN scalar';
