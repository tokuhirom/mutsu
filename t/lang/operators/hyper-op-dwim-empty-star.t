use Test;

# Two refinements to hyper-op dwim length semantics:
#   1. A dwim (cycling) side that is empty cannot be filled, so any empty
#      operand makes a dwimmy hyper return an empty list (not a 0-padded one).
#      A non-dwimmy length mismatch still throws X::HyperOp::NonDWIM.
#   2. A list ending in `*` (Whatever) is "infinitely extensible by copying its
#      last real element": it adapts to the other side's length, padding with
#      its last real element instead of cycling.
# Mirrors roast/S03-metaops/hyper.t.

plan 16;

# --- empty operand against a dwim side -> empty ---
is (True «+« ()).gist, '()', 'left-dwim, empty RHS -> empty';
is (True »+» ()).gist, '()', 'right-dwim, empty RHS -> empty';
is (True «+» ()).gist, '()', 'both-dwim, empty RHS -> empty';
is (() «+« True).gist, '()', 'left-dwim, empty LHS -> empty';
is (() »+» True).gist, '()', 'right-dwim, empty LHS -> empty';
my @a = <a b c d e>;
is (@a «+« ()).elems, 0, 'dwim against empty list -> empty (list operand)';
is (() «+» @a).elems, 0, 'empty dwim list against a list -> empty';

# --- non-dwim against empty still throws X::HyperOp::NonDWIM ---
throws-like { True »+« () }, X::HyperOp::NonDWIM,
    left-elems => 1, right-elems => 0, 'non-dwim against empty RHS throws';
throws-like { () »+« True }, X::HyperOp::NonDWIM,
    left-elems => 0, right-elems => 1, 'non-dwim against empty LHS throws';

# --- `*`-terminated list extends by copying its last real element ---
is ~(<A B C D E> »~» (1, 2, 3, *)), 'A1 B2 C3 D3 E3',
    'right-dwim: `*` list pads with last real element';
is ~(<A B C D E> «~» (1, 2, 3, *)), 'A1 B2 C3 D3 E3',
    'both-dwim: `*` list pads with last real element';
is ~((1, 2, 3, *) «~« <A B C D E>), '1A 2B 3C 3D 3E',
    'left-dwim: `*` list pads with last real element';
is ~((1, 2, 3, *) «~» <A B C D E>), '1A 2B 3C 3D 3E',
    'both-dwim: `*` list on the left pads';
is ~((1, 2, *) «~» (4, 5, *)), '14 25',
    'both `*` lists: result length is the longer real prefix';

# --- ordinary cycling dwim is unaffected ---
is ((1, 2, 3, 4) <<+>> (10, 20)).gist, '(11 22 13 24)', 'shorter dwim side still cycles';
is ((1, 2, 3) >>+>> (10, 20, 30)).gist, '(11 22 33)', 'equal lengths unaffected';
