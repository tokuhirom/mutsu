use Test;
use MONKEY-SEE-NO-EVAL;

plan 6;

# The exit phasers of an EVAL'd unit run when the unit ends.
my @l;
EVAL Q[ENTER { @l.push("enter") }; LEAVE { @l.push("leave") }; @l.push("body")];
is @l.join(","), "enter,body,leave", 'LEAVE of an EVAL unit runs after its body';

my @k;
EVAL Q[KEEP { @k.push("keep") }; UNDO { @k.push("undo") }; @k.push("body"); 42];
is @k.join(","), "body,keep", 'KEEP runs for a defined value, UNDO does not';

my @u;
EVAL Q[KEEP { @u.push("keep") }; UNDO { @u.push("undo") }; @u.push("body"); Nil];
is @u.join(","), "body,undo", 'UNDO runs for an undefined value';

my @order;
EVAL Q[LEAVE { @order.push("first") }; LEAVE { @order.push("second") }; 1];
is @order.join(","), "second,first", 'several LEAVE phasers run last-written first';

my @t;
EVAL Q[multi trait_mod:<is>(Variable:D $v, :$unit!) {
    $v.block.add_phaser("ENTER", { @t.push("enter") });
    $v.block.add_phaser("LEAVE", { @t.push("leave") });
}
my $u is unit; @t.push("body")];
is @t.join(","), "enter,body,leave", 'a variable trait can add a unit-level LEAVE phaser';

my @d;
try EVAL Q[LEAVE { @d.push("leave") }; die "boom"];
is @d.join(","), "leave", 'LEAVE runs when the unit dies';
