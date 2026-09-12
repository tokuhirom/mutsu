use v6;
use Test;

# A user `multi infix:<op>` extends the operator's candidate set only for the
# operators rakudo actually DECLARES as `&infix:<op>` (ADR-0071). For the names
# rakudo has no infix routine for -- `cross`, `zip`, `roundrobin`, which are
# core list-op `sub`s there, so `(1,2) cross (3,4)` is "Two terms in a row" --
# the declaration installs a FRESH lexical routine that SHADOWS whatever the
# name answered before. A call its candidates do not accept is then
# X::Multi::NoMatch naming only the user's signatures, not a silent
# fall-through to the builtin. See docs/adr/0093 and issue #8006.
#
# Every expectation below was measured against rakudo. Each row runs in its own
# EVAL, with its own class name, so one row cannot leak into the next.

plan 23;

# --- a core-only name: the user declaration shadows it outright --------------

throws-like { EVAL 'class Q1 { }; multi infix:<cross>(Q1 $a, Q1 $b) { "QC" }; (1,2) cross (3,4)' },
    X::Multi::NoMatch,
    'a call no user infix:<cross> candidate accepts is X::Multi::NoMatch';

is EVAL('class Q2 { }; multi infix:<cross>(Q2 $a, Q2 $b) { "QC" }; '
        ~ 'try { (1,2) cross (3,4) }; $!.message.lines[0]'),
    'Cannot resolve caller infix:<cross>(List:D, List:D); none of these signatures matches:',
    'and the message names the call profile the way rakudo does';

is EVAL('class Q3 { }; multi infix:<cross>(Q3 $a, Q3 $b) { "QC" }; '
        ~ 'try { (1,2) cross (3,4) }; $!.message.lines[1].trim'),
    '(Q3 $a, Q3 $b)',
    'and lists only the user candidate, not a core one';

is EVAL('class Q4 { }; multi infix:<cross>(Q4 $a, Q4 $b) { "QC" }; Q4.new cross Q4.new'), 'QC',
    'a call the user candidate does accept still reaches it';

throws-like { EVAL 'class Q5 { }; multi infix:<zip>(Q5 $a, Q5 $b) { "QZ" }; (1,2) zip (3,4)' },
    X::Multi::NoMatch,
    'infix:<zip> is core-only too: a non-matching call does not fall through';
is EVAL('class Q6 { }; multi infix:<zip>(Q6 $a, Q6 $b) { "QZ" }; Q6.new zip Q6.new'), 'QZ',
    "and infix:<zip>'s own candidate is still reachable";

throws-like
    { EVAL 'class Q7 { }; multi infix:<roundrobin>(Q7 $a, Q7 $b) { "QR" }; (1,2) roundrobin (3,4)' },
    X::Multi::NoMatch,
    'infix:<roundrobin> is core-only too';
is EVAL('class Q8 { }; multi infix:<roundrobin>(Q8 $a, Q8 $b) { "QR" }; '
        ~ 'Q8.new roundrobin Q8.new'), 'QR',
    "and infix:<roundrobin>'s own candidate is still reachable";

# The `X` spelling is a separate name with its own core candidates, so a
# `cross` declaration leaves it alone.
is EVAL('class Q9 { }; multi infix:<cross>(Q9 $a, Q9 $b) { "QC" }; ((1,2) X (3,4)).gist'),
    '((1 3) (1 4) (2 3) (2 4))',
    'declaring infix:<cross> does not touch the separate infix:<X>';

# A `where` clause that declines is the Math::Vector shape this was found on:
# the core list operator must not answer for the rejected call.
my $V = 'class V%s { has @.e; method dim { @!e.elems } }; '
      ~ 'multi infix:<cross>(V%s $a where { $a.dim == 3 }, V%s $b where { $b.dim == 3 }) { "3D" }; ';
is EVAL(sprintf($V, 'A', 'A', 'A')
        ~ 'infix:<cross>(VA.new(e => (1,2,3)), VA.new(e => (4,5,6)))'), '3D',
    'a where-constrained user infix:<cross> candidate matches when the guard holds';
throws-like { EVAL sprintf($V, 'B', 'B', 'B')
        ~ 'infix:<cross>(VB.new(e => (1,2,3,4,5)), VB.new(e => (5,4,3,2,1)))' },
    X::Multi::NoMatch,
    'and a call the guard rejects dies instead of reaching the core list operator';

# --- the operators rakudo DOES declare keep their core candidate set ---------
#
# These are the contrast rows: `+`, `minmax`, `min`, `max`, `eqv`, `x`, `cmp`,
# `~` and `==` are all real `&infix:<...>` routines in rakudo, so a call the
# user's candidate does not accept still runs the core implementation.

is EVAL('class P1 { }; multi infix:<+>(P1 $a, P1 $b) { "P" }; 1 + 2'), 3,
    'infix:<+> is a real core multi: 1 + 2 keeps working';
is EVAL('class P2 { }; multi infix:<minmax>(P2 $a, P2 $b) { "P" }; ((1,2) minmax (3,4)).gist'),
    '1..4',
    'infix:<minmax> is a real core multi, so the core candidate still answers';
is EVAL('class P3 { }; multi infix:<min>(P3 $a, P3 $b) { "P" }; 1 min 2'), 1,
    'infix:<min> keeps its core candidate';
is EVAL('class P4 { }; multi infix:<max>(P4 $a, P4 $b) { "P" }; 1 max 2'), 2,
    'infix:<max> keeps its core candidate';
is EVAL('class P5 { }; multi infix:<eqv>(P5 $a, P5 $b) { "P" }; (1,2) eqv (1,2)'), True,
    'infix:<eqv> keeps its core candidate';
is EVAL('class P6 { }; multi infix:<x>(P6 $a, P6 $b) { "P" }; "a" x 3'), 'aaa',
    'infix:<x> keeps its core candidate';
is EVAL('class P7 { }; multi infix:<cmp>(P7 $a, P7 $b) { "P" }; (1 cmp 2).gist'), 'Less',
    'infix:<cmp> keeps its core candidate';
is EVAL('class P8 { }; multi infix:<~>(P8 $a, P8 $b) { "P" }; "a" ~ "b"'), 'ab',
    'infix:<~> keeps its core candidate';
is EVAL('class P9 { }; multi infix:<==>(P9 $a, P9 $b) { "P" }; 1 == 1'), True,
    'infix:<==> keeps its core candidate';

# A routine merely DECLARED in a module and never exported is not a candidate
# set the caller can see, so it must not turn the call into an X::Multi::NoMatch
# naming signatures that are not in scope -- the name stays undeclared here.
# (roast S06-operator-overloading/imported-subs.t pins the same shape with a
# module fixture; this row keeps it from regressing through the shadowing path.)
throws-like { EVAL '3 notthere 4' }, X::Syntax::Confused,
    'an operator no routine in scope declares is still a syntax error';

# A plain `sub` (not `multi`) has always shadowed the operator outright, for a
# real core operator as much as for a core-only name -- unchanged here.
is EVAL('sub infix:<cross>($a, $b) { "SUB" }; (1,2) cross (3,4)'), 'SUB',
    'a plain sub infix:<cross> replaces the operator for every argument type';
is EVAL('sub infix:<+>($a, $b) { "SUB" }; 1 + 2'), 'SUB',
    'a plain sub infix:<+> replaces the operator too';

done-testing;
