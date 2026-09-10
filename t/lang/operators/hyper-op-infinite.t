use Test;

# A hyper operator (`>>op<<` and dwim variants) whose result length is
# determined by an infinite/lazy operand throws X::HyperOp::Infinite with a
# `side` attribute naming the infinite operand(s). A non-dwimmy length
# mismatch throws X::HyperOp::NonDWIM with left-elems/right-elems.
# Mirrors roast/S03-metaops/hyper.t.

plan 16;

# --- OK cases: infinite operand on the sole dwim (cycling) side, bounded by a
#     finite non-dwim side, produces a finite result ---
{
    my @r = (0 xx *) <<+<< (1..9);
    is ~@r, ~<1 2 3 4 5 6 7 8 9>, 'infinite left cycles up to finite right (<<op<<)';

    my @s = (1..9) >>+>> (0 xx *);
    is ~@s, ~<1 2 3 4 5 6 7 8 9>, 'infinite right cycles up to finite left (>>op>>)';
}

# --- Infinite, left infinite ---
throws-like { (0 xx *) <<+>> (1..9) }, X::HyperOp::Infinite, side => 'left',
    'both dwim, left infinite';
throws-like { (0 xx *) >>+>> (1..9) }, X::HyperOp::Infinite, side => 'left',
    'right dwim, left fixed+infinite';
throws-like { (0 xx *) >>+<< (1..9) }, X::HyperOp::Infinite, side => 'left',
    'both non-dwim, left infinite';

# --- Infinite, right infinite ---
throws-like { (1..9) <<+>> (0 xx *) }, X::HyperOp::Infinite, side => 'right',
    'both dwim, right infinite';
throws-like { (1..9) <<+<< (0 xx *) }, X::HyperOp::Infinite, side => 'right',
    'left dwim, right fixed+infinite';
throws-like { (1..9) >>+<< (0 xx *) }, X::HyperOp::Infinite, side => 'right',
    'both non-dwim, right infinite';

# --- Infinite, both infinite (any arrow) ---
throws-like { (1..Inf) >>+<< (1..Inf) }, X::HyperOp::Infinite, side => 'both',
    'both non-dwim, both infinite';
throws-like { (1..Inf) <<+>> (1..Inf) }, X::HyperOp::Infinite, side => 'both',
    'both dwim, both infinite';
throws-like { (1..Inf) <<+<< (1..Inf) }, X::HyperOp::Infinite, side => 'both',
    'left dwim, both infinite';
throws-like { (1..Inf) >>+>> (1..Inf) }, X::HyperOp::Infinite, side => 'both',
    'right dwim, both infinite';

# --- NonDWIM length mismatch carries element counts ---
throws-like { (1,2,3) >>~<< <A B C D E> }, X::HyperOp::NonDWIM,
    left-elems => 3, right-elems => 5,
    'non-dwim length mismatch reports element counts';

# --- finite operations still work and dwim variants extend/truncate ---
{
    my @r = (1,2,3) <<~>> <A B C D E>;
    is ~@r, ~<1A 2B 3C 1D 2E>, 'both dwim extends shorter side';
    my @s = (1,2,3,4) >>~>> <A B C D E>;
    is ~@s, ~<1A 2B 3C 4D>, 'right dwim truncates to left length';
    is-deeply (1,2,3) >>+<< (10,20,30), (11, 22, 33), 'both non-dwim equal length works';
}
