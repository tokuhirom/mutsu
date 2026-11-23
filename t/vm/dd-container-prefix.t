use Test;

# #18985: `dd` prefixes a line the way rakudo's `dd` does. A container
# argument shows its name (`$x = 5`), and a declared type goes in front
# (`Int $y = Int`). An `@`/`%` variable is not itself a container, so it shows
# only a declared element type. A value with no container (a literal, a
# parameter, a `:=`-bound value, `self`) shows nothing.

plan 13;

my $captured = '';
my class Capture {
    method print(*@parts) { $captured ~= @parts.join; True }
    method say(*@parts)   { $captured ~= @parts.join ~ "\n"; True }
}
sub dd-of(&code) {
    $captured = '';
    { my $*ERR = Capture.new; code() }
    $captured.chomp
}

my $x = 5;
is dd-of({ dd $x }), '$x = 5', 'a scalar shows its name';
my @a = 1, 2;
is dd-of({ dd @a }), '[1, 2]', 'an array shows only its value';
my %h = a => 1;
is dd-of({ dd %h }), '{:a(1)}', 'a hash shows only its value';
is dd-of({ dd 42 }), '42', 'a literal shows only its value';
my Int $y;
is dd-of({ dd $y }), 'Int $y = Int', 'a typed scalar shows its type';
my Str $s = "q";
is dd-of({ dd $s }), 'Str $s = "q"', 'a typed scalar holding a value';
my $z;
is dd-of({ dd $z }), '$z = Any', 'an untyped empty scalar';
my Str %sh;
is dd-of({ dd %sh }), 'Str  = (my Str %)', 'a typed hash shows only its element type';
sub f($p) { dd $p }
is dd-of({ f(3) }), '3', 'a parameter is no container';
my $l := 7;
is dd-of({ dd $l }), '7', 'a value bound with := is no container';
my $*D = 4;
is dd-of({ dd $*D }), '$*D = 4', 'a dynamic variable shows its twigil';
my class C { has $.a = 1; method m { dd $!a } }
is dd-of({ C.new.m }), '$!a = 1', 'an attribute shows its twigil';
is dd-of({ dd $x, @a }), "\$x = 5\n[1, 2]", 'each argument is its own line';
