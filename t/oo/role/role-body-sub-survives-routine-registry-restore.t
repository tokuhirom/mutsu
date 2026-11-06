use Test;

# A role body's `sub` closes over the composition's parameters. The class
# composing it is built once and cached; a routine that declares inner routines
# (here via a LEAVE phaser) restores the routine registry on exit, which used
# to drop the class-package copy of the sub, so the second construction found
# only the role's own sub with no `$prefix` (Marrow t/02-db.t, via DB::SQLite's
# BitEnum, #16899).

plan 5;

role BE[Str:D :$prefix = '', Bool :$lc] {
    sub lookup(Str:D $s is copy) {
        $s .= uc if $lc;
        "$prefix$s"
    }
    method get($n) { lookup($n) }
}

sub open-leave {
    LEAVE { }
    my $f = BE[:lc, prefix => 'P_'].new;
    $f.get('a')
}
sub open-undo {
    UNDO { }
    my $f = BE[:lc, prefix => 'Q_'].new;
    $f.get('b')
}

is open-leave(), 'P_A', 'first use composed inside a LEAVE routine';
is open-leave(), 'P_A', 'second use of the same specialization';
is open-undo(), 'Q_B', 'a different specialization, composed inside a routine with UNDO';
is open-undo(), 'Q_B', 'and its second use';
is BE[:lc, prefix => 'P_'].new.get('c'), 'P_C', 'a construction outside any routine';
