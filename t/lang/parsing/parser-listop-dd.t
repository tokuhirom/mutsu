use Test;

# #19082: `dd` is a list operator like `note`, so inside parentheses it takes
# the whole comma list: `(dd 1, 2)` is `(dd(1, 2))`, not `(dd(1), 2)`.

plan 3;

my $err = '';
my class Capture {
    method print(*@a) { $err ~= @a.join; True }
    method say(*@a)   { $err ~= @a.join ~ "\n"; True }
}

{
    my $*ERR = Capture.new;
    my @x = (dd 1, 2);
    is @x.elems, 1, 'the parenthesized dd call is one element';
}
is $err, "1\n2\n", 'dd received both arguments';

$err = '';
{
    my $*ERR = Capture.new;
    my @y = (note 3, 4);
}
is $err, "34\n", 'note behaves the same way';
