# Every routine shape ADR-0110's typed, resolved IR (TRIR) admits, plus the
# boundary cases it must decline to. Printed as one `key=value` line per
# shape so `t/vm/codegen/adr0110-trir-differential.t` can run this file twice — once
# with TRIR on, once with MUTSU_TRIR=off — and require the two to agree
# exactly. See ADR-0110 §5 ("Semantics drift vs the general binder").
#
# Anything printed here must be deterministic and must not depend on the
# order the two runs happen in.
use nqp;

my $ws := nqp::list_i;
nqp::bindpos_i($ws, 32, 1);   # space
nqp::bindpos_i($ws, 9, 1);    # tab
nqp::push_i($ws, 0);

# --- the `nom-ws` shape: str + native `is rw` int + a free variable ---------
my sub nom-ws(str $text, int $pos is rw --> Nil) {
    nqp::while(nqp::atpos_i($ws, nqp::ordat($text, $pos)), ++$pos);
}

my str $doc = "a \t  b";
my int $p = 1;
nom-ws($doc, $p);
say "nom-ws-from-1={$p}";
$p = 5;
nom-ws($doc, $p);
say "nom-ws-from-5={$p}";
$p = 0;
nom-ws($doc, $p);
say "nom-ws-from-0={$p}";

# A second string through the same routine: the per-frame character memo must
# not serve one string's codepoints for another.
my str $other = "  zz";
my int $q = 0;
nom-ws($other, $q);
say "nom-ws-other={$q}";
$p = 1;
nom-ws($doc, $p);
say "nom-ws-doc-again={$p}";

# --- no free variable ------------------------------------------------------
my sub skip-spaces(str $text, int $pos is rw) {
    nqp::while(nqp::iseq_i(nqp::ordat($text, $pos), 32), ++$pos);
}
my int $r = 1;
skip-spaces("a   b", $r);
say "skip-spaces={$r}";

# --- reading past the end --------------------------------------------------
# `nqp::ordat` answers -1 past the end and `nqp::atpos_i` answers 0 out of
# range, so the scan must stop rather than run away.
my int $far = 99;
nom-ws($doc, $far);
say "nom-ws-past-end={$far}";

# --- native int arithmetic, comparison and a returned value ----------------
my sub arith(int $a, int $b) {
    nqp::add_i(nqp::mul_i($a, $b), nqp::sub_i($a, $b));
}
say "arith={arith(7, 3)}";
say "arith-neg={arith(-7, 3)}";

my sub cmps(int $a, int $b) {
    nqp::add_i(
      nqp::mul_i(nqp::islt_i($a, $b), 100),
      nqp::add_i(nqp::mul_i(nqp::iseq_i($a, $b), 10), nqp::isgt_i($a, $b)))
}
say "cmps-lt={cmps(1, 2)}";
say "cmps-eq={cmps(2, 2)}";
say "cmps-gt={cmps(3, 2)}";

# Raku-spelled operators on native operands lower to the same typed ops.
my sub raku-ops(int $a, int $b) { $a * $b + ($a - $b) }
say "raku-ops={raku-ops(7, 3)}";

# Native `int` wraps rather than promoting (ADR-0110 §3.2).
my sub wrap-mul(int $a, int $b) { nqp::mul_i($a, $b) }
say "wrap={wrap-mul(9223372036854775807, 2)}";

# --- nqp::if / nqp::stmts --------------------------------------------------
my sub classify(int $n) {
    nqp::if(nqp::islt_i($n, 0), -1, nqp::if(nqp::iseq_i($n, 0), 0, 1))
}
say "classify={classify(-5)},{classify(0)},{classify(5)}";

my sub counted(str $text, int $pos is rw --> Nil) {
    nqp::stmts(
      nqp::while(nqp::iseq_i(nqp::ordat($text, $pos), 32), ++$pos),
      nqp::if(nqp::iseq_i(nqp::ordat($text, $pos), 122), ++$pos)
    );
}
my int $c = 0;
counted("  zab", $c);
say "counted={$c}";

# --- native `num` ----------------------------------------------------------
my sub halve(num $x) { nqp::div_n($x, 2e0) }
say "halve={halve(7e0)}";
my sub num-cmp(num $x, num $y) { nqp::islt_n($x, $y) }
say "num-cmp={num-cmp(1e0, 2e0)},{num-cmp(2e0, 1e0)}";

# --- `chars` ---------------------------------------------------------------
my sub width(str $s) { nqp::chars($s) }
say "width={width('hello')},{width('')}";

# --- boundary: what the binder must still reject ---------------------------
# A native `int` parameter accepts a `Bool` (Bool does Int) and rejects a
# `Str`; a native `str` parameter rejects a non-string. TRIR must raise the
# same exception the general binder does, or decline to it.
my sub takes-int(int $n) { nqp::add_i($n, 1) }
say "int-from-bool={takes-int(True)},{takes-int(False)}";
say "int-from-str={(try takes-int('7')) // 'FAILED: ' ~ ($! ~~ Exception ?? $!.^name !! 'unknown')}";
say "int-from-type={(try takes-int(Int)) // 'FAILED: ' ~ ($! ~~ Exception ?? $!.^name !! 'unknown')}";

my sub takes-str(str $s) { nqp::chars($s) }
say "str-from-int={(try takes-str(42)) // 'FAILED: ' ~ ($! ~~ Exception ?? $!.^name !! 'unknown')}";

# A very large `Int` argument is outside native `int` range.
say "int-huge={(try takes-int(10**30)) // 'FAILED'}";

# --- `is rw` through a closure-captured container --------------------------
# The caller's variable is boxed into a shared cell, so the writeback has to
# go THROUGH it or the closure would keep seeing the stale value.
my int $shared = 1;
my $peek = -> { $shared };
nom-ws($doc, $shared);
say "rw-through-cell={$shared},{$peek()}";

# --- arity mismatch --------------------------------------------------------
say "arity={(try arith(1)) // 'FAILED'}";

# --- `.wrap` (ADR-0110 §3.3's run-time guard) ------------------------------
# A statically linked call site would step straight past the wrapper, so the
# guard has to send the call back to the ordinary dispatch. Kept LAST: once
# anything in this program is wrapped, every TRIR call site consults the
# wrapper table, so an earlier shape would measure the guarded path instead of
# the linked one.
my sub wrappable(int $n) { nqp::add_i($n, 1) }
say "wrap-before={wrappable(1)}";
&wrappable.wrap(-> $n { callsame() * 10 });
say "wrap-after={wrappable(1)}";
