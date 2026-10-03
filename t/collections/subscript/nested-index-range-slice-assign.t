use Test;
# From Terminal::UI (Terminal::ANSI::Virtual.print-at): `@.chars[$r][$c..^$e] = $str.comb`.
plan 6;
my @a = [], []; @a[1][1..2] = <A B>;
is-deeply @a.raku, '[[], [Any, "A", "B"]]', 'inclusive range, nested element';
my @b = [], []; @b[1][1..^3] = <A B>;
is-deeply @b.raku, '[[], [Any, "A", "B"]]', 'range excluding end';
my @c = [1,2],[3,4]; @c[1][0..1] = <X Y>;
is-deeply @c.raku, '[[1, 2], ["X", "Y"]]', 'overwrite existing elements';
class V {
  has @.chars;
  method pa($r, $c, $s) { @.chars[$r] //= []; @.chars[$r][$c..^($c + $s.chars)] = $s.comb }
}
my $v = V.new; $v.pa(1, 1, 'AB');
is-deeply $v.chars[1].raku, '$[Any, "A", "B"]', 'attribute array, autovivified row';
my $r = []; $r[1..2] = <A B>;
is-deeply $r.raku, '$[Any, "A", "B"]', 'single level unchanged';
my @d = [], []; @d[1][0..2] = 1, 2;
is-deeply @d[1].raku, '$[1, 2, Any]', 'short RHS leaves tail unset';
