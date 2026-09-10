use v6;
use Test;

# A `for` loop whose block declares zero POSITIONAL parameters still gets handed
# one element per iteration, so rakudo dies on the very first invocation --
# before the body runs once -- with
#   "Too many positionals passed; expected 0 arguments but got 1".
# An empty source invokes the block zero times and is therefore fine.
#
# This must hold in every position the loop can appear in: as a plain statement,
# as a value-collecting expression, and in statement-modifier spelling. The
# expression forms used to silently run the body instead (the whole point of
# this file), and the statement form reported the source length rather than the
# one-element chunk it actually passes.
#
# NOTE: independent cases are kept at file scope with unique names rather than
# wrapped in bare `{ ... }` blocks -- see t/closure-capture-nested-named-sub.t.

plan 35;

my $msg = 'Too many positionals passed; expected 0 arguments but got 1';

# --- plain statement position ------------------------------------------------

my $stmt-ran = 0;
dies-ok { for 1, 2, 3, 4 -> { $stmt-ran++ } },
    'statement `for LIST -> { }` dies';
is $stmt-ran, 0, 'statement `for LIST -> { }` never ran the body';

try { for 1, 2, 3, 4 -> { 1 } };
is $!.message, $msg,
    'statement form reports the one-element chunk, not the source length';

# --- expression (value-collecting) position ----------------------------------

my $expr-ran = 0;
dies-ok { my @r = (for 1, 2, 3, 4 -> { $expr-ran++ }); @r },
    'expression `(for LIST -> { })` dies';
is $expr-ran, 0, 'expression `(for LIST -> { })` never ran the body';

try { my $x = (for 1, 2, 3, 4 -> { 1 }); $x };
is $!.message, $msg, 'expression form reports the one-element chunk';

my $do-ran = 0;
dies-ok { my @r = do for 1, 2, 3 -> { $do-ran++ }; @r },
    '`do for LIST -> { }` dies';
is $do-ran, 0, '`do for LIST -> { }` never ran the body';

my $elems-ran = 0;
dies-ok { (for 1, 2, 3 -> { $elems-ran++ }).elems },
    'a `for` expression consumed by a method call dies';
is $elems-ran, 0, 'method-consumed `for` expression never ran the body';

# --- statement-modifier position ---------------------------------------------

my $mod-ran = 0;
dies-ok { -> { $mod-ran++ } for 1, 2, 3, 4 },
    'statement-modifier `-> { } for LIST` dies';
is $mod-ran, 0, 'statement-modifier form never ran the body';

my $mod-expr-ran = 0;
dies-ok { my @r = (-> { $mod-expr-ran++ } for 1, 2, 3, 4); @r },
    'statement-modifier `for` in expression position dies';
is $mod-expr-ran, 0, 'statement-modifier expression form never ran the body';

try { my @r = (-> { 1 } for 1, 2, 3, 4); @r };
is $!.message, $msg, 'statement-modifier expression form reports one argument';

my $sub-ran = 0;
dies-ok { sub () { $sub-ran++ } for 1, 2, 3 },
    'statement-modifier `sub () { } for LIST` dies';
is $sub-ran, 0, '`sub () { } for LIST` never ran the body';

# --- an EMPTY source invokes the block zero times, so it is fine -------------

my $empty-ran = 0;
lives-ok { for () -> { $empty-ran++ } },
    'statement `for () -> { }` lives (block never invoked)';
is $empty-ran, 0, 'empty statement source ran no iterations';

my @empty-result = (for () -> { 99 });
is @empty-result.elems, 0, 'expression `(for () -> { })` yields no elements';

my @empty-src;
my @empty-var-result = (for @empty-src -> { 99 });
is @empty-var-result.elems, 0, 'an empty array source is fine too';

# --- a NAMED slurpy is invisible to the positional count, so it is zero-count -

my $named-slurpy-ran = 0;
dies-ok { for 1, 2, 3 -> *%h { $named-slurpy-ran++ } },
    'statement `for LIST -> *%h { }` dies (zero positionals)';
is $named-slurpy-ran, 0, '`-> *%h` never ran the body';

my $named-slurpy-expr-ran = 0;
dies-ok { my @r = (for 1, 2, 3 -> *%h { $named-slurpy-expr-ran++ }); @r },
    'expression `(for LIST -> *%h { })` dies';
is $named-slurpy-expr-ran, 0, 'expression `-> *%h` never ran the body';

lives-ok { for () -> *%h { 1 } }, '`for () -> *%h { }` lives';

# A named slurpy BESIDE a positional still binds that positional normally.
my @with-positional = (for 1, 2, 3 -> $a, *%h { $a * 10 });
is @with-positional.join(','), '10,20,30',
    '`-> $a, *%h` keeps its one positional and runs';

# --- a block with NO signature binds the topic and is NOT zero-count ---------

my @topic-expr = (for 1, 2, 3 { $_ * 2 });
is @topic-expr.join(','), '2,4,6', 'a bare block in expression position uses $_';

my @topic-mod = ({ $_ * 3 } for 1, 2, 3);
is @topic-mod.join(','), '3,6,9', 'a bare block as statement modifier uses $_';

my @one-param = (for 1, 2, 3 -> $a { $a + 1 });
is @one-param.join(','), '2,3,4', '`-> $a` in expression position still works';

my @one-param-mod = (-> $a { $a + 5 } for 1, 2, 3);
is @one-param-mod.join(','), '6,7,8', '`-> $a` as statement modifier still works';

# --- `lazy for` throws only once the resulting Seq is reified ----------------

my $lazy-ran = 0;
my $lazy-seq = (lazy for 1, 2, 3 -> { $lazy-ran++ });
is $lazy-ran, 0, '`lazy for ... -> { }` has not run anything yet';
dies-ok { $lazy-seq.eager }, 'reifying a `lazy for ... -> { }` dies';

# `lazy for` with a real signature keeps working (the loop's ParamDefs, defaults
# included, reach the lowered gather loop).
my $lazy-pairs = (lazy for 1, 2, 3 -> $a, $b = 7 { $a + $b });
is $lazy-pairs.eager.join(','), '3,10',
    '`lazy for` honours a multi-param signature with a default';

my @lazy-rw = 1, 2, 3;
my $lazy-rw-seq = (lazy for @lazy-rw <-> $v { $v = $v * 10 });
$lazy-rw-seq.eager;
is @lazy-rw.join(','), '10,20,30', '`lazy for` keeps its `<->` write-back';
