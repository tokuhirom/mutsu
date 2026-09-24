use v6;
use nqp;
use Test;

# The positional nqp:: ops resolve an index through ONE rule
# (`nqp_backing::resolve_index`, ADR-0118): a negative index counts from the
# end, and one before the start dies, as MoarVM's VMArray does. Each op used
# to clamp a negative index to 0 -- `nqp::bindpos($l, -1, $v)` overwrote the
# FIRST element -- or read it as absent. And `AT-POS`/`ASSIGN-POS` are the
# same routines as `[]`/`[]=`. Expected values were measured with rakudo.

plan 27;

sub dies-with(&code, $message, $desc) {
    # `sink` makes a returned Failure throw, as a died call does.
    try { sink code() }
    is ($! ?? $!.message !! 'did not die'), $message, $desc;
}

my $l := nqp::list(1, 2, 3);
is nqp::atpos($l, -1), 3, 'atpos -1 is the last element';
is nqp::atpos($l, -3), 1, 'atpos -len is the first';
dies-with { nqp::atpos($l, -4) }, 'MVMArray: Index out of bounds', 'atpos before the start dies';
ok nqp::isnull(nqp::atpos($l, 9)), 'atpos past the end is null';

my $li := nqp::list_i(1, 2, 3);
is nqp::atpos_i($li, -1), 3, 'atpos_i -1';
dies-with { nqp::atpos_i($li, -4) }, 'MVMArray: Index out of bounds', 'atpos_i before the start dies';
is nqp::atpos_i($li, 9), 0, 'atpos_i past the end is 0';

my $ls := nqp::list_s("a", "b");
is nqp::atpos_s($ls, -1), "b", 'atpos_s -1';
dies-with { nqp::atpos_s($ls, -3) }, 'MVMArray: Index out of bounds', 'atpos_s before the start dies';

{ my $b := nqp::list(1, 2, 3); nqp::bindpos($b, -1, 9);
  is nqp::atpos($b, 2), 9, 'bindpos -1 writes the last element (it wrote the first)';
  is nqp::atpos($b, 0), 1, '... and leaves the first alone' }
dies-with { nqp::bindpos(nqp::list(1), -9, 9) }, 'MVMArray: Index out of bounds', 'bindpos before the start dies';
{ my $b := nqp::list_i(1, 2, 3); nqp::bindpos_i($b, -2, 7); is nqp::atpos_i($b, 1), 7, 'bindpos_i -2' }
{ my $b := nqp::list_s("a", "b"); nqp::bindpos_s($b, -1, "z"); is nqp::atpos_s($b, 1), "z", 'bindpos_s -1' }
{ my $b := nqp::list_i(); nqp::bindpos_i($b, 2, 1); is nqp::atpos_i($b, 0), 0, 'a bindpos_i gap reads 0' }

{ my $sp := nqp::list(1, 2, 3, 4); nqp::splice($sp, nqp::list("x"), -2, 1);
  is nqp::atpos($sp, 2) ~ nqp::elems($sp), "x4", 'splice at -2 counts from the end' }
dies-with { nqp::splice(nqp::list(1), nqp::list(), -5, 1) }, 'MVMArray: Illegal splice offset',
    'splice before the start dies';

{ my $b := buf8.new(1, 2, 255); is nqp::atpos_i($b, -1), 255, 'a Buf resolves the index the same way' }

# -- AT-POS is `[]`, ASSIGN-POS is `[]=` --
my @a = 1, 2, 3;
my $neg = -1;
dies-with { @a.AT-POS(-1) }, 'Index out of range. Is: -1, should be in 0..^Inf', 'AT-POS -1 is an OutOfRange';
dies-with { @a[$neg] }, 'Index out of range. Is: -1, should be in 0..^Inf', '... as [-1] is';
my Int @i = 1, 2;
is @i.AT-POS(5).raku, 'Int', 'AT-POS past the end of a typed array is its type (it was Any)';
is @i[5].raku, 'Int', '... as [] is';
is "abc".AT-POS(0), "abc", 'Str.AT-POS(0) is the one-element-list rule';
isa-ok "abc".AT-POS(1), Failure, 'Str.AT-POS(1) is out of range (it indexed a character)';
{ my @c; @c.ASSIGN-POS(3, 1); is-deeply (@c[0]:exists, @c.elems), (False, 4), 'an ASSIGN-POS gap is a hole, as with []=' }
dies-with { my @c = 1, 2; @c.ASSIGN-POS(-1, 5) }, 'Index out of range. Is: -1, should be in 0..^Inf', 'ASSIGN-POS -1 refuses';
{ my @x = 1, 2; my @y := @x; @x.ASSIGN-POS(1, 9); is @y[1], 9, 'ASSIGN-POS writes through the shared container' }
