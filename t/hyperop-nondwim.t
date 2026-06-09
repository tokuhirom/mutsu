use Test;

plan 5;

# A non-dwimmy hyper op (»op«) over lists of differing lengths raises
# X::HyperOp::NonDWIM carrying left-elems, right-elems, and an `operator`
# routine handle whose .name is the infix name.

throws-like '<a b> »+« <c>', X::HyperOp::NonDWIM,
    left-elems => 2, right-elems => 1,
    operator => { .name eq 'infix:<+>' };

throws-like '(1, 2, 3) »*« (1, 2)', X::HyperOp::NonDWIM,
    left-elems => 3, right-elems => 2,
    operator => { .name eq 'infix:<*>' };

throws-like '<a b> »+« <c>', X::HyperOp::NonDWIM,
    message => /:s not of the same length/;

# Equal-length non-dwimmy hyper ops still work.
is-deeply (1, 2, 3) »+« (10, 20, 30), (11, 22, 33),
    'equal-length non-dwimmy hyperop works';

# A dwimmy side cycles/pads, so unequal lengths are allowed.
is-deeply (1, 2, 3, 4) »+» (10, 20), (11, 22, 13, 24),
    'dwimmy hyperop cycles the shorter side';
