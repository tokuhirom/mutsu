use Test;

plan 6;

# A `my $x := $b` (or a plain `my $x = $b`) in one loop body must not leak
# into a later loop whose block exit (an ENTER / FIRST region) syncs aliases:
# the later loop's `$b` was read as the previous loop's last value.
my @a;
for [1,2,3] -> $b { my $x := $b; ENTER { 1 }; @a.push: $b }
is @a.raku, '[1, 2, 3]', 'bind with ENTER';

my @e;
for [1,2,3] -> $b { my $x := $b; FIRST { 1 }; @e.push: $b }
is @e.raku, '[1, 2, 3]', 'bind with FIRST, after an earlier loop';

my @f;
for [1,2,3] -> $b { my $x = $b; FIRST { 1 }; @f.push: $b }
is @f.raku, '[1, 2, 3]', 'plain copy with FIRST, after earlier loops';

my @g;
for 1,2,3 -> $b { my $x := $b; ENTER { 1 }; @g.push: "b=$b x=$x" }
is @g.join(' '), 'b=1 x=1 b=2 x=2 b=3 x=3', 'a third bind loop with ENTER';
my @h;
for 1,2,3 -> $b { my $x := $b; ENTER { 1 }; @h.push: "b=$b x=$x" }
is @h.join(' '), 'b=1 x=1 b=2 x=2 b=3 x=3', 'an identical loop right after it';

my @k;
for [4,5] -> $b { @k.push: $b; FIRST { 1 } }
is @k.raku, '[4, 5]', 'a loop without any binding is unaffected';
