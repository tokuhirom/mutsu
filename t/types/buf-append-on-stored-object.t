use v6;
use Test;

# A Buf reached through an accessor, a hash element or an array element is the
# object its container holds, so push/append/unshift/prepend grow that object
# (Net::BGP's `self.buffer.append($buf)` on a `has buf8:D $.buffer`).

plan 12;

class C {
    has buf8:D $.buffer = buf8.new;
    has $.plain = buf8.new;
}
my $c = C.new;
$c.buffer.append(1, 2, 3);
is $c.buffer.elems, 3, 'append through a typed accessor grows the attribute';
$c.buffer.push(4);
is $c.buffer.list, (1, 2, 3, 4), 'push through the accessor';
$c.buffer.prepend(0);
is $c.buffer.list, (0, 1, 2, 3, 4), 'prepend through the accessor';
$c.buffer.unshift(9);
is $c.buffer.elems, 6, 'unshift through the accessor';
$c.plain.append(7, 8);
is $c.plain.list, (7, 8), 'append through an untyped accessor';

my %h = b => buf8.new;
%h<b>.append(1, 2);
is %h<b>.elems, 2, 'append on a hash element';

my @l = buf8.new;
@l[0].append(5);
is @l[0].elems, 1, 'append on an array element';

my $a = buf8.new(1);
my $b = $a;
$b.append(2);
is $a.elems, 2, 'an assigned Buf is the same object (rakudo: reference semantics)';
is $b.elems, 2, 'and the alias grew too';

is buf8.new(1, 2).append(3).elems, 3, 'append on a temporary answers the grown buffer';
throws-like { blob8.new(1).append(2) }, Exception, 'a Blob stays immutable';
my $bl = blob8.new(1, 2);
is $bl.elems, 2, 'and is unchanged';
