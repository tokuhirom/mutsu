use Test;

# A `do` block (and an EVAL'd unit) yields its tail variable's container,
# read after the LEAVE/KEEP/UNDO phasers ran, so a phaser that changes the
# variable changes the block's value. A routine decontainerizes its return
# value before LEAVE, so its value is the one before the phaser (#17894).

plan 9;

is do { my $x = 3; LEAVE { $x = 4 }; $x }, 4, 'a do block\'s tail is read after LEAVE';
use MONKEY-SEE-NO-EVAL;
is EVAL(q[my $x = 3; LEAVE { $x = 4 }; $x]), 4, '... and an EVAL\'d unit\'s';
sub routine { my $x = 3; LEAVE { $x = 4 }; $x }
is routine(), 3, 'a routine returns the value before LEAVE';
is do { my $y = 1; KEEP { $y = 7 }; $y }, 7, 'a KEEP phaser (defined tail) changes it';
is do { my $u = Int; UNDO { $u = 5 }; $u }, 5, 'an UNDO phaser (undefined tail) changes it';
my $o = 1;
is do { LEAVE { $o = 2 }; $o }, 2, 'an outer variable as the tail';
is do { my $z = 3; LEAVE { $z = 4 }; $z + 0 }, 3, 'an expression tail is read before LEAVE';
my $v = do { my $w = 3; LEAVE { $w = 4 }; $w };
$v = 9;
is $v, 9, 'the do block\'s value is a copy, assignable on its own';
is-deeply do { my @a = 1, 2; LEAVE { @a.push: 3 }; @a }, [1, 2, 3], 'an array tail';
