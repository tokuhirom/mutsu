use v6;
use Test;

# The renderers (`.gist`, `.raku`, `.Str`) keep the ancestors of the node they
# are on so that a cycle is named instead of recursed into. That stack answers
# "is this node an ancestor?" in O(1) once it is deep (#12714); the answers it
# gives, for shallow and deep structures, cycles and shared nodes, are pinned.

plan 13;

my @a;
@a.push(@a);
like @a.gist, /^ '(\\Array_' \d+ ' = [Array_' \d+ '])' $/, 'a self-containing array gists as a back-reference';
like @a.raku, /^ '((my @Array_' \d+ ') = [@Array_' \d+ ',])' $/, '... and .raku names it';

my %h;
%h<self> = %h;
like %h.gist, /^ '(\\Hash_' \d+ ' = {self => Hash_' \d+ '})' $/, 'a self-containing hash gists as a back-reference';
like %h.raku, /^ '((my %Hash_' \d+ ') = {:self(%Hash_' \d+ ')})' $/, '... and .raku names it';

my @d = 1, 2;
my @o = @d, @d;
is @o.gist, '[[1 2] [1 2]]', 'a node shared by two non-nested paths is rendered in full both times (gist)';
is @o.raku, '[[1, 2], [1, 2]]', '... and in .raku';

my $deep = [];
$deep = [$deep] for ^300;
is $deep.gist.chars, 602, 'a 300-deep nesting gists in full';
is $deep.raku.chars, 903, '... and .raku renders every level';
is $deep.Str.chars, 0, '... and .Str of nested empty arrays is empty';

# Past the depth where the ancestor stack switches to its hash index.
my $deeper = [];
$deeper = [$deeper] for ^5000;
is $deeper.gist.chars, 10002, 'a 5000-deep nesting gists in full';
is $deeper.raku.chars, 15003, '... and .raku renders every level';

my @e = [1, [2, [3, @d]]];
@d.push(@e);
ok @e.gist.chars > 0, 'a cycle that closes two levels down still terminates (gist)';
ok @e.raku.chars > 0, '... and (raku)';
