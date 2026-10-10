use v6.d;
use Test;

plan 13;

# Rakudo runs a variable trait handler while compiling the declaration, so
# nothing here logs from the handler itself: only the phasers it adds do.
my @log;

multi trait_mod:<is>(Variable:D $v, :$foo!) {
    $v.block.add_phaser("ENTER", { @log.push("enter") });
}
multi trait_mod:<is>(Variable:D $v, :$kind!) {
    $v.block.add_phaser("ENTER", { @log.push($v.block.^name) });
}

{ my Int $x is foo; }
is @log.elems, 1, 'Variable.block.add_phaser("ENTER") runs when the block is entered';

@log = ();
sub f { my $y is foo; }
f(); f(); f();
is @log.elems, 3, 'the ENTER phaser runs on every entry of the block';

@log = ();
sub nested { my $a is foo; { my $b is foo; } }
nested();
is @log.elems, 2, 'a nested block carries its own phaser';

@log = ();
{ my $k is kind; }
is-deeply @log, ["Block"], 'Variable.block is a Block';

# The phaser can seed the variable the trait was applied to.
multi trait_mod:<is>(Variable:D $v, :$seeded!) {
    $v.block.add_phaser("ENTER", { $v.var = 42 without $v.var });
}
sub g { my $z is seeded; $z }
is g(), 42, 'the phaser assigns the variable through Variable.var';
is g(), 42, '... on a second entry too';

# The phaser runs in the block's own ENTER queue, before the body.
my @order;
multi trait_mod:<is>(Variable:D $v, :$first!) {
    $v.block.add_phaser("ENTER", { @order.push("enter") });
}
sub ordered { @order.push("body"); my $o is first; @order.push("after"); }
ordered();
is-deeply @order, ["enter", "body", "after"], 'the phaser runs before the block body';

# The exit kinds run on block exit, KEEP/UNDO by the outcome.
my @exit;
multi trait_mod:<is>(Variable:D $v, :$exits!) {
    $v.block.add_phaser("LEAVE", { @exit.push("leave") });
    $v.block.add_phaser("KEEP", { @exit.push("keep") });
    $v.block.add_phaser("UNDO", { @exit.push("undo") });
}
sub ok-exit { my $e is exits; @exit.push("body"); 1 }
ok-exit();
is-deeply @exit, ["body", "keep", "leave"], 'LEAVE and KEEP run on a normal exit';
@exit = ();
sub bad-exit { my $e is exits; @exit.push("body"); die "x" }
try bad-exit();
is-deeply @exit, ["body", "undo", "leave"], 'LEAVE and UNDO run when the block dies';
@exit = ();
ok-exit(); ok-exit();
is @exit.elems, 6, 'the exit phasers run on every entry';

# A phaser kind a block queue does not take is refused.
multi trait_mod:<is>(Variable:D $v, :$bad!) {
    $v.block.add_phaser("NOPE", { });
}
throws-like { EVAL 'my $q is bad;' }, Exception, 'an unsupported phaser kind is refused';
multi trait_mod:<is>(Variable:D $v, :$lateleave!) {
    $v.block.add_phaser("LEAVE", { });
}
throws-like { EVAL 'my $q is lateleave;' }, Exception, 'a LEAVE phaser from an unlifted trait is refused';

# Variable.block does not break the other reflective methods.
my $name;
multi trait_mod:<is>(Variable:D $v, :$named!) { $v.block.add_phaser("ENTER", { $name = $v.name }) }
{ my $w is named; }
is $name, '$w', 'Variable.name still works';
