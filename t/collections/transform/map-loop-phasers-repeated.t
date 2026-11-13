use Test;

plan 8;

# A `.map` block's loop phasers run once per element (NEXT) or per map
# (FIRST, LAST), however many times the block is mapped and however many
# closures share its body (#17337: the phaser bodies are compiled once).

my $next = 0;
my $first = 0;
my $last = 0;
my $block = { FIRST $first++; NEXT $next++; LAST $last++; $_ * 2 };

is (^5).map($block).List, (0, 2, 4, 6, 8), 'the block maps';
is $next, 5, 'NEXT ran once per element';
is "$first $last", '1 1', 'FIRST and LAST ran once';

(^3).map($block).elems for ^4;
is $next, 17, 'NEXT keeps counting across repeated maps';
is $last, 5, 'LAST ran once per map';

# Two closures of one body each see their own captured variable.
sub make-counter() {
    my $seen = 0;
    my $b = { NEXT $seen++; $_ };
    $b, -> { $seen }
}
my ($b1, $c1) = make-counter();
my ($b2, $c2) = make-counter();
(^3).map($b1).elems;
(^7).map($b2).elems;
is "{$c1()} {$c2()}", '3 7', 'each closure steps its own variable';

# A NEXT phaser that maps with a phaser block of its own.
my $inner = 0;
my $outer-block = { NEXT (^2).map({ NEXT $inner++; $_ }).elems; $_ };
(^3).map($outer-block).elems;
is $inner, 6, 'a nested map in a NEXT phaser runs its own NEXT';

# grep runs NEXT too.
my $gnext = 0;
is (^6).grep({ NEXT $gnext++; $_ %% 2 }).List.elems, 3, 'grep with NEXT filters';
