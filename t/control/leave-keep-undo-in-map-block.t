use Test;

plan 10;

# LEAVE / KEEP / UNDO phasers in a map / grep callback run once per call of
# the block, like in a `for` body (zef 1.1.4's Zef::Repository::Ecosystems
# relies on this: `.map(-> $uri { UNDO ...; KEEP ...; LEAVE ...; next ... }).head`).

my @l;

(1, 2).map({ LEAVE @l.push("a$_"); $_ }).eager;
is @l.join(' '), 'a1 a2', 'LEAVE in a bare map block';
@l = ();

my @r = <a b>.map(-> $x { LEAVE @l.push("leave $x"); next if $x eq 'a'; $x });
is @l.join(', '), 'leave a, leave b', 'LEAVE fires on next and on normal exit';
is @r.join(','), 'b', 'next still skips the element';
@l = ();

<a b>.map(-> $x { KEEP @l.push("keep $x"); UNDO @l.push("undo $x"); next if $x eq 'a'; $x }).eager;
is @l.join(', '), 'undo a, keep b', 'UNDO on next, KEEP on a defined value';
@l = ();

my $head = <a b c>.map(-> $x { LEAVE @l.push("l$x"); next if $x eq 'a'; "v$x" }).head;
is $head, 'vb', '.head of a map with a LEAVE phaser';
ok @l.head eq 'la', 'LEAVE ran for the skipped element before .head returned';
@l = ();

my &f = -> $x { LEAVE @l.push("c$x"); $x };
(4, 5).map(&f).eager;
is @l.join(' '), 'c4 c5', 'LEAVE in a named pointy block passed to map';
@l = ();

my @g = (1, 2, 3).grep(-> $x { LEAVE @l.push("g$x"); $x != 2 });
is @l.join(' '), 'g1 g2 g3', 'LEAVE in a grep block';
is @g.join(','), '1,3', 'grep result is unchanged';
@l = ();

my @v = (1, 2).map({ LEAVE @l.push("x"); my $y = $_ * 10; $y + 1 });
is @v.join(','), '11,21', 'the block value is kept when it has a LEAVE';
