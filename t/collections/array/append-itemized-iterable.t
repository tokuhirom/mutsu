use Test;

# A native array's one-argument `append` adds the elements of an
# Iterable argument even when it is itemized (rakudo's `array.append(\value)`),
# while an `Array` keeps an itemized argument whole. PDF's LZW filter appends
# `@table[$code]` (an itemized Array) to a `my uint8 @out` (#14232). Expected
# values are the `raku` (v2026.09) output.

plan 7;

my $x = [1, 2];
my int @o;
@o.append: $x;
is @o.join(','), '1,2', 'an itemized Array argument adds its elements';

my @t = [[2],];
@t[1] = @t[0].clone;
my uint8 @u;
@u.append: @t[1];
is @u.join(','), '2', '... an itemized Array element too';

my int @p = 9;
dies-ok { @p.prepend: $x }, 'prepend keeps an itemized argument whole, and so dies';

my int @l;
@l.append: $(3, 4);
is @l.join(','), '3,4', 'an itemized List argument adds its elements';

my @a;
@a.append: $x;
is @a.raku, '[[1, 2],]', 'an Array keeps an itemized argument whole';

my int @m;
dies-ok { @m.append($x, 3) }, 'several arguments are not flattened';

my int @n;
@n.append: 5;
is @n.join(','), '5', 'a single non-Iterable argument is one element';
