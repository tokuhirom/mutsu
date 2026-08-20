use Test;

# `box_captured_lexicals` / `box_decl_local_cell` / `atomic_scalar_cell`
# refuse to promote a captured/declared local into a shared `ContainerRef`
# cell when its current value is an Array, Hash, Package, Sub, or Proxy --
# those types are already reference-shared (Gc/Arc-backed), so a scalar-level
# cell is unnecessary. Seq/HyperSeq/RaceSeq/Slip are Arc-backed the same way,
# so they now share the same refusal (see
# todo/tickets/atomic-cell-shape-refusal-asymmetry.md): a captured local
# whose value transitions from a refused shape (e.g. Array) to a Seq shape
# (e.g. via `flat`) no longer triggers a mid-sequence promotion, so it stays
# on the general shared_vars cross-thread lane for its whole lifetime instead
# of switching mechanisms partway through.

plan 4;

# 1. The ticket's exact motivating shape: cas alternates a captured lexical
# between an Array (round 1) and the Seq that round 1's `flat` produced
# (round 2), across two sequential awaited rounds.
{
    my $acc = [];
    Promise.allof(start { cas $acc, -> @c { flat @c, 1 } }).result;
    Promise.allof(start { cas $acc, -> @c { flat @c, 2 } }).result;
    is ~$acc, '1 2',
        'sequential cas rounds stay coherent across an Array->Seq shape transition';
}

# 2. Plain (non-cas) reassignment of a captured Seq across an awaited thread
# boundary stays coherent via the general shared_vars reconcile now that Seq
# is refused cell promotion.
{
    my $seen = (1, 2, 3).Seq;
    Promise.allof(start { $seen = flat($seen, 4) }).result;
    is ~$seen, '1 2 3 4',
        'reassigned captured Seq visible after await without cell promotion';
}

# 3. Same for Slip.
{
    my $s = slip(1, 2, 3);
    Promise.allof(start { $s = slip($s, 4) }).result;
    is ~$s, '1 2 3 4',
        'reassigned captured Slip visible after await without cell promotion';
}

# 4. A legacy-lane cas primes a value while the variable holds a refused
# (Array) shape, then a NEW closure captures the same name via a plain
# reassignment while it holds another refused (Seq) shape: since neither
# shape is cell-promotable, both stay on the same (legacy/shared_vars) lane
# throughout instead of a mid-sequence promotion disconnecting one from the
# other.
{
    my $x = [1, 2, 3];
    cas $x, -> @c { flat @c, 4 };
    Promise.allof(start { $x = flat($x, 5) }).result;
    is ~$x, '1 2 3 4 5',
        'legacy-lane cas followed by a plain reassignment inside a new thread stays coherent';
}
