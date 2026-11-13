use Test;

# `map BLOCK, LIST` / `grep BLOCK, LIST` with several list arguments keeps
# each argument's container: the topic aliases it, as `.map` on a List does.

plan 7;

sub h($x is rw) { $x = "H:$x"; 42 }

my $a = 1; my $b = 2;
my @r = map { h($_) }, $a, $b;
is "$a $b", 'H:1 H:2', 'an is rw call on the topic writes the arguments';
is @r, [42, 42], 'the results';

$a = 1; $b = 2;
@r = map { $_ = 'Z' }, $a, $b;
is "$a $b", 'Z Z', 'assigning the topic writes the arguments';

$a = 1; $b = 2;
@r = map -> $x is rw { $x = 'P' }, $a, $b;
is "$a $b", 'P P', 'an is rw pointy parameter aliases the arguments';

$a = 1; $b = 2;
@r = grep { $_ = 'G'; True }, $a, $b;
is "$a $b", 'G G', 'grep aliases the topic too';

{
    my $*F = 1; my $*G = 2;
    sub d { my @x = map { h($_) }, $*F, $*G; }
    d();
    is "$*F $*G", 'H:1 H:2', 'dynamic variables of a caller frame are aliased';
}

throws-like { my @x = map { $_ = 5 }, 1, 2 }, Exception,
    'a literal element still cannot be assigned';
