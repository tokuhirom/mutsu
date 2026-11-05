use v6;
use Test;

plan 5;

# `x[i] := v` is carried through finalization as the parser built it
# (ADR-15760 slice 2); the bound value keeps the container binding.

my @a = 1, 2, 3;
my $v = 9;
@a[0] := $v;
$v = 5;
is @a.raku, '[5, 2, 3]', 'a positional element is bound to the variable';

my %h;
my $w = 1;
%h<k> := $w;
$w = 7;
is %h.raku, '{:k(7)}', 'an associative element is bound to the variable';

%h<p> := 1 => 2;
is %h<p>.raku, '1 => 2', 'the pair is the bound value, not the bind result';

my @b;
my $x = 3;
@b[1] := $x;
$x++;
is @b[1], 4, 'a bind into an empty array autovivifies the slot';

my %n;
my $y = 'a';
%n{'q'} := $y;
$y ~= 'b';
is %n<q>, 'ab', 'a braced key binds as well';
