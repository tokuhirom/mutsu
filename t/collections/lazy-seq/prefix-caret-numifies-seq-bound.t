use Test;

# Prefix `^` numifies its bound, so a Seq counts its items (Terminal::Boxer's
# `^$cell-height` with `$cell-height = max @content».lines`); the infix
# `0..^$seq` still refuses a Seq endpoint.
plan 5;

my $s = "a\nb\nc".lines;
is (^$s).list.join(','), '0,1,2', '^ of a Seq counts its items';

my $got = '';
for ^"x\ny".lines -> $i { $got ~= $i }
is $got, '01', '^ of a method-call Seq in a for loop';

sub lines-of($t) { $t.lines }
is (^lines-of("p\nq\nr\ns")).elems, 4, '^ of a call result';

my $n = 3;
is (^$n).list.join(','), '0,1,2', 'an Int bound is unchanged';

throws-like { my $t = (1, 2).map(* + 1); (0..^$t).list }, X::Range::InvalidArg,
  'the infix range still refuses a Seq endpoint';
