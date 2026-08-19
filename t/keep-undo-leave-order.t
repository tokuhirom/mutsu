use Test;
plan 6;

# A loop body's LEAVE phaser used to run BEFORE KEEP/UNDO on normal
# (uninterrupted) completion, instead of after -- the opposite order from
# real raku. See todo/tickets/loop-body-leave-runs-before-keep-undo-instead-of-after.md.
#
# Verified against real raku (Rakudo 2026.06):
#   raku -e 'my $s = ""; for 1 { LEAVE { $s ~= "L" }; KEEP { $s ~= "K" };
#             UNDO { $s ~= "U" }; 1 }; say $s'
#   => KL
# and the KEEP-vs-UNDO selection is by definedness of the trailing value
# (defined -> KEEP, undefined -> UNDO), not by its truthiness:
#   raku -e '... UNDO { ... }; Nil }; say $s' => UL

# `for` loop, single iteration, defined trailing value -> KEEP, then LEAVE.
my $s1 = "";
for 1 {
    LEAVE { $s1 ~= "L" }
    KEEP { $s1 ~= "K" };
    UNDO { $s1 ~= "U" };
    1;
}
is $s1, "KL", 'for-loop: KEEP runs before LEAVE on normal completion';

# `for` loop, single iteration, undefined trailing value -> UNDO, then LEAVE.
my $s2 = "";
for 1 {
    LEAVE { $s2 ~= "L" }
    KEEP { $s2 ~= "K" };
    UNDO { $s2 ~= "U" };
    Nil;
}
is $s2, "UL", 'for-loop: UNDO runs before LEAVE on normal completion';

# `for` loop, multiple iterations, all normal completion with KEEP.
my $s3 = "";
for 1, 2, 3 {
    LEAVE { $s3 ~= "L" }
    KEEP { $s3 ~= "K" };
    UNDO { $s3 ~= "U" };
    1;
}
is $s3, "KLKLKL", 'for-loop: KEEP-then-LEAVE order holds on every iteration';

# `while` loop, defined trailing value -> KEEP, then LEAVE.
my $s4 = "";
my $i = 0;
while $i < 1 {
    $i++;
    LEAVE { $s4 ~= "L" }
    KEEP { $s4 ~= "K" };
    UNDO { $s4 ~= "U" };
    1;
}
is $s4, "KL", 'while-loop: KEEP runs before LEAVE on normal completion';

# `while` loop, undefined trailing value -> UNDO, then LEAVE.
my $s5 = "";
$i = 0;
while $i < 1 {
    $i++;
    LEAVE { $s5 ~= "L" }
    KEEP { $s5 ~= "K" };
    UNDO { $s5 ~= "U" };
    Nil;
}
is $s5, "UL", 'while-loop: UNDO runs before LEAVE on normal completion';

# C-style `loop`, defined trailing value -> KEEP, then LEAVE.
my $s6 = "";
loop (my $j = 0; $j < 1; $j++) {
    LEAVE { $s6 ~= "L" }
    KEEP { $s6 ~= "K" };
    UNDO { $s6 ~= "U" };
    1;
}
is $s6, "KL", 'C-style loop: KEEP runs before LEAVE on normal completion';
