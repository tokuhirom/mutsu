use Test;

# One `whenever` literal instantiated many times shares its compiled chunk and
# its phaser split (#7667). The sharing must be invisible: each instantiation
# still gets its own emitter, its own captured lexicals, and its own phasers.

plan 6;

sub doubler(Supply $in) { supply { whenever $in -> $v { emit $v * 2; LAST emit 'end' } } }

# Two live instances of the SAME literal must not answer each other's emitter.
my $a = Supplier.new;
my $b = Supplier.new;
my (@ga, @gb);
doubler($a.Supply).tap({ @ga.push($_) });
doubler($b.Supply).tap({ @gb.push($_) });
$a.emit(1); $b.emit(10); $a.emit(2); $b.emit(20);
is @ga, [2, 4],   'instance A saw only its own source';
is @gb, [20, 40], 'instance B saw only its own source';

# The LAST phaser is per instance, and fires on its own source's done.
$a.done;
is @ga, [2, 4, 'end'], "instance A's LAST fired on its own done";
is @gb, [20, 40],      "instance B's LAST did not fire";
$b.done;
is @gb, [20, 40, 'end'], "instance B's LAST fired on its own done";

# A lexical captured by the literal is per instantiation, not shared.
sub counter(Supply $in) { supply { my $n = 0; whenever $in -> $v { $n++; emit $n } } }
my $c = Supplier.new;
my $d = Supplier.new;
my (@gc, @gd);
counter($c.Supply).tap({ @gc.push($_) });
counter($d.Supply).tap({ @gd.push($_) });
$c.emit('x'); $c.emit('x'); $c.emit('x'); $d.emit('y');
is (@gc, @gd), ([1, 2, 3], [1]), 'each instantiation has its own captured lexical';
