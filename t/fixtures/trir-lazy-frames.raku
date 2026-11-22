# Fixture for t/vm/codegen/adr8673-trir-lazy-frames.t: TRIR routines call each
# other (`outer` -> `middle` -> a leaf), so the leaf's and `middle`'s
# routine-stack entries are deferred (ADR-8673 D1). Every leaf then does one
# thing that observes the routine stack, and prints what it saw. The run must
# be identical with the entries pushed eagerly, lazily, and with TRIR off.
use nqp;

# The routines on the call stack whose names end in `$suffix`, innermost first.
sub trail($suffix) { Backtrace.new.list.map(*.subname).grep(*.ends-with($suffix)).join(',') }

# --- a call out of TRIR: the callee reads its callers' frames ---------------
sub look-up($x) { trail('-call') ~ ' / ' ~ callframe(1).code.name }
sub leaf-call($x) { look-up($x) }
sub middle-call($x) { leaf-call($x) }
sub outer-call($x) { middle-call($x) }
say "callframe=", outer-call(1);

# --- nqp::die in a leaf: the backtrace names every TRIR routine -------------
sub leaf-die($x) { nqp::die("boom $x") }
sub middle-die($x) { leaf-die($x) }
sub outer-die($x) { middle-die($x) }
for 1, 2 {
    outer-die($_);
    CATCH {
        default {
            say "die=", .message, " ", .backtrace.list.map(*.subname).grep(/'-die'/).join(',');
        }
    }
}

# --- a user `.Bool` reached from a truth test in a leaf ---------------------
class Observer {
    method Bool() { say "bool-callers=", trail('-bool'); True }
}
sub leaf-bool($o) { $o ?? 'yes' !! 'no' }
sub middle-bool($o) { leaf-bool($o) }
sub outer-bool($o) { middle-bool($o) }
say "bool=", outer-bool(Observer.new);

# --- a user `.Str` reached from a concatenation in a leaf -------------------
class Shown {
    method Str() { trail('-concat') }
}
sub leaf-concat($o) { "str=" ~ $o }
sub middle-concat($o) { leaf-concat($o) }
sub outer-concat($o) { middle-concat($o) }
say outer-concat(Shown.new);

# --- a dynamic variable read in a leaf -------------------------------------
sub leaf-dyn($x) { $*LAZY-DYN }
sub middle-dyn($x) { leaf-dyn($x) }
sub outer-dyn($x) { my $*LAZY-DYN = "dyn-$x"; middle-dyn($x) }
say "dynamic=", outer-dyn(7);

# --- EVAL reached from a leaf ----------------------------------------------
sub evaluate($code) { EVAL $code }
sub leaf-eval($x) {
    evaluate('Backtrace.new.list.map(*.subname).grep(/"-eval"/).join(",")')
}
sub middle-eval($x) { leaf-eval($x) }
sub outer-eval($x) { middle-eval($x) }
say "eval=", outer-eval(1);

# --- a wrapped leaf: the wrapper sees its callers --------------------------
sub leaf-wrapped($x) { "leaf-$x" }
sub middle-wrapped($x) { leaf-wrapped($x) }
sub outer-wrapped($x) { middle-wrapped($x) }
say "before-wrap=", outer-wrapped(1);
&leaf-wrapped.wrap(-> $x { "wrapped(" ~ callsame() ~ ")" });
say "after-wrap=", outer-wrapped(2);

# --- an exception from a callee: its backtrace -----------------------------
sub thrower($x) { die "thrown $x" }
sub leaf-throw($x) { thrower($x) }
sub middle-throw($x) { leaf-throw($x) }
sub outer-throw($x) { middle-throw($x) }
try { outer-throw(3) }
say "throw=", $!.message, " ", $!.backtrace.list.map(*.subname).grep(/'throw'/).join(',');

# --- a warning raised in a leaf: its location ------------------------------
sub leaf-warn($x) { warn "careful $x"; $x }
sub middle-warn($x) { leaf-warn($x) }
sub outer-warn($x) { middle-warn($x) }
# (Printed to stderr by the default handler, with its backtrace.)
say "warn=", outer-warn(4);

# --- a pure chain: nothing observes, the values come back -------------------
sub leaf-pure(int $x --> int) { $x * 2 }
sub middle-pure(int $x --> int) { leaf-pure($x) + 1 }
sub outer-pure(int $x --> int) { middle-pure($x) + 1 }
say "pure=", outer-pure(20);
