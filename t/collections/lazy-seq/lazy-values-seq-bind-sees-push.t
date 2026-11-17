use Test;

# A lazy `.values` / `IterationBuffer.Seq` Seq bound with `:=` stays unread
# until it is consumed, so an element pushed before then shows through, as
# rakudo's Seq iterates its source only when consumed (#17938).

plan 5;

my @a = 1, 2, 3;
my $s := @a.values;
@a.push(4);
is $s.elems, 4, 'a bound @a.values sees a later push';

my $ib := IterationBuffer.new;
$ib.push($_) for 1..3;
my $q := $ib.Seq;
$ib.push(9);
is $q.elems, 4, 'a bound IterationBuffer.Seq sees a later push';

my @c = 1, 2;
my $v := @c.values;
@c.push(3);
is $v.join(','), '1,2,3', '... and reads the pushed element';

my $x = [Any].values;
is $x.raku, '$((Any,).Seq)', 'a one-Any .values Seq stays a Seq';

my @b = 1, 2;
my $t = @b.values;
@b.push(3);
is $t.elems, 3, 'an assigned .values Seq too, as rakudo does';
