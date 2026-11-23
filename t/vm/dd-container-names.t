use Test;

# #19046: `dd` names the argument's container the way rakudo's `dd` does from
# `$var.VAR`. An `is rw` parameter and a `:=` alias are the caller's
# container, so they report its name and type. An `is copy` parameter's
# container is typed `Any`. An element of a named array or hash is
# `element of @a`.

plan 11;

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
sub c($p is copy) { dd $p }
is dd-of({ c(1) }), 'Any $p = 1', 'an is copy parameter is typed Any';
sub r($p is rw) { dd $p }
is dd-of({ r($x) }), '$x = 5', 'an is rw parameter names the caller';
my $m := $x;
is dd-of({ dd $m }), '$x = 5', 'a := alias names the aliased container';
my Int $t = 3;
my $tt := $t;
is dd-of({ dd $tt }), 'Int $t = 3', '... with its type';

my @a = 1, 2;
is dd-of({ dd @a[0] }), 'element of @a = 1', 'an array element';
my %h = a => 1;
is dd-of({ dd %h<a> }), 'element of %h = 1', 'a hash element';
my Int @ti = 1, 2;
is dd-of({ dd @ti[1] }), 'Int element of @ti = 2', 'a typed array element';
my Str %hs = a => "x";
is dd-of({ dd %hs<a> }), 'Str element of %hs = "x"', 'a typed hash element';
is dd-of({ dd [7][0] }), '7', 'an element of an anonymous array has no prefix';
is dd-of({ dd @a[0], 9 }), "element of @a = 1\n9", 'an element next to a literal';

sub cv($p is copy) { $p.VAR.of }
is cv(1).^name, 'Any', 'and its .VAR.of is Any';
