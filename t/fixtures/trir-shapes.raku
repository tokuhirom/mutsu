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

# --- `substr` / `eqat` ------------------------------------------------------
# JSON::Fast's `parse-string` fast path is exactly this shape: `eqat` to spot
# a clean quote and `substr` to slice the token out, both against the SAME
# document text repeatedly (issue #8900).
my sub slice(str $s, int $from, int $len) { nqp::substr($s, $from, $len) }
say "substr-mid={slice('hello world', 6, 5)}";
say "substr-clip={slice('hi', 0, 99)}";
say "substr-oob={slice('hi', 99, 5)}";
say "substr-neg={slice('hi', -3, 2)}";
say "substr-empty={slice('hi', 1, 0)}";

my sub starts-with(str $s, str $needle, int $pos) { nqp::eqat($s, $needle, $pos) }
say "eqat-hit={starts-with('hello', 'ell', 1)}";
say "eqat-miss={starts-with('hello', 'ell', 0)}";
say "eqat-at-end={starts-with('hello', 'o', 4)}";
say "eqat-oob={starts-with('hi', 'longneedle', 0)}";
say "eqat-neg-pos={starts-with('hi', 'h', -1)}";

# Same document text reused across both ops and across `ordat`/`chars`, so the
# per-frame char memo has to answer all of them consistently.
my sub scan-token(str $s, int $pos) {
    nqp::if(
      nqp::eqat($s, '"', nqp::sub_i($pos, 1)),
      nqp::substr($s, $pos, nqp::sub_i(nqp::chars($s), $pos)),
      'no-quote'
    )
}
my str $quoted = Q["] ~ 'abcxyz';
say "scan-token={scan-token($quoted, 1)}";

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

# --- a parameter whose BINDING the general binder alone performs -----------
# An attributive parameter writes straight to `self`'s attribute cell. Its
# body here is empty, so the routine is otherwise perfectly provable — which
# is exactly why the gate has to be on the parameter's spelling rather than on
# what the body does with it.
class Attributive {
    has $!t = 'orig';
    method set($v) { sub s($!t) { }; s($v); }
    method get { $!t }
}
my $box = Attributive.new;
$box.set('bound');
say "attributive-param={$box.get}";

# --- Stage 2: a TRIR body that calls out -----------------------------------
# The resolved form (`CallTr`). `bump` is already compiled when `bump-twice`
# is, so its signature is known and its native `is rw` parameter takes a
# reference to the CALLER's slot rather than a copy.
my sub bump(int $pos is rw --> Nil) { ++$pos }
my sub bump-twice(int $pos is rw --> Nil) { nqp::stmts(bump($pos), bump($pos)) }
my int $b = 10;
bump-twice($b);
say "call-tr-rw={$b}";

# Three frames deep, so the reference is PASSED ON rather than only handed
# down once — the case an in-and-out copy would get wrong.
my sub bump-thrice(int $pos is rw --> Nil) { nqp::stmts(bump-twice($pos), bump($pos)) }
$b = 0;
bump-thrice($b);
say "call-tr-rw-deep={$b}";

# Recursion through the resolved form: a routine's own chunk is registered
# before its body compiles, which is what `parse-thing` needs.
my sub fact(int $n) {
    nqp::if(nqp::islt_i($n, 2), 1, nqp::mul_i($n, fact(nqp::sub_i($n, 1))))
}
say "recursion={fact(10)}";

# The generic form (`CallGen`): `late` is declared AFTER `early`, so at the
# call site there is no signature to compile against — exactly `nom-ws`
# calling `nom-comment`.
my sub early(int $n) { late($n) + 1 }
my sub late(int $n) { $n * 2 }
say "call-gen-forward={early(5)}";

# A generic callee that WRITES its argument: with no signature to consult the
# caller must pass the variable as a container and read it back.
my sub gen-bump($pos is rw) { $pos = $pos + 7 }
my sub via-gen(int $pos is rw --> Nil) { gen-bump($pos) }
my int $g = 1;
via-gen($g);
say "call-gen-rw={$g}";

# --- Stage 2: declarations inside a TRIR body ------------------------------
my sub span-of-spaces(str $text, int $pos is rw) {
    nqp::stmts(
      (my int $start = $pos),
      nqp::while(nqp::iseq_i(nqp::ordat($text, $pos), 32), ++$pos),
      nqp::sub_i($pos, $start)
    )
}
my int $d = 0;
say "decl-int={span-of-spaces('   x', $d)},{$d}";

# --- Stage 2: throwing out of a TRIR frame ---------------------------------
# Every one of `JSON::Fast`'s scanners ends in a `die` helper, so the cold
# path has to unwind a TRIR frame correctly — and leave the frame stacks in a
# state the NEXT call can still use.
my sub boom(str $text, int $pos) { die "boom at $pos in $text" }
my sub scan-or-die(str $text, int $pos is rw) {
    nqp::stmts(
      nqp::while(nqp::iseq_i(nqp::ordat($text, $pos), 32), ++$pos),
      nqp::if(nqp::iseq_i(nqp::ordat($text, $pos), 33), boom($text, $pos), $pos)
    )
}
my int $s1 = 0;
say "scan-ok={scan-or-die('  a', $s1)}";
my int $s2 = 0;
say "scan-die={(try scan-or-die('  !', $s2)) // 'FAILED: ' ~ $!.Str}";
my int $s3 = 0;
say "scan-after-die={scan-or-die('   b', $s3)},{$s3}";

# --- Stage 2: a dynamic variable read --------------------------------------
my $*TRIR-SCALE = 3;
my sub reads-dyn(int $n) { $*TRIR-SCALE * $n }
say "dyn-read={reads-dyn(4)}";

# --- Stage 2: a list literal in value position -----------------------------
my sub pair-of(int $n) { ($n, nqp::add_i($n, 1)) }
say "list-literal={pair-of(4).join(',')}";

# --- Stage 2: a trailing `if` is the routine's value ------------------------
# Compiling the branches for effect and returning `Nil` is a wrong answer, not
# a missing optimization. Called twice on purpose: the first call runs untyped,
# because a call site is linked to TRIR only once it has executed.
my sub sel2($a, $b) { if $a && $b { 'both' } elsif $a || $b { 'one' } else { 'none' } }
say "trailing-if={sel2(True, True)},{sel2(True, False)},{sel2(False, False)}";
say "trailing-if-again={sel2(True, True)},{sel2(True, False)},{sel2(False, False)}";
my sub sel0($a) { if $a { 'yes' } }
say "trailing-if-no-else={sel0(True)},{sel0(False).raku},{sel0(True)}";

# An `if` in SINK position is still compiled, and the routine's value is the
# statement after it.
my sub after-if(int $n) {
    if $n > 0 { my $unused = 1 }
    nqp::add_i($n, 1)
}
say "sink-if={after-if(1)},{after-if(1)},{after-if(-1)}";

# --- Stage 2: a parameter is read-only unless `is rw` ----------------------
# A typed slot store cannot raise `X::Assignment::RO`, so a body that writes
# one of its own read-only parameters has to decline to the general binder
# rather than quietly writing the slot. Called twice: the first call runs
# untyped, so a TRIR-only regression hides behind it.
my sub writes-param($x) { $x = 1; 'wrote' }
say "ro-param-1={(try writes-param(1)) // 'FAILED'}";
say "ro-param-2={(try writes-param(1)) // 'FAILED'}";
my sub bumps-param(int $n) { $n++; $n }
say "ro-param-incr-1={(try bumps-param(1)) // 'FAILED'}";
say "ro-param-incr-2={(try bumps-param(1)) // 'FAILED'}";
# The `is rw` counterpart still writes.
my sub writes-rw($x is rw) { $x = 5 }
my $rw = 0;
writes-rw($rw);
my $rw2 = 0;
writes-rw($rw2);
say "rw-param={$rw},{$rw2}";

# --- literal-name `nqp::getattr` / `bindattr` sites (ADR-0121 D3) ----------
# The name is resolved when the chunk is compiled and a bareword class operand
# is remembered per registry generation; every receiver kind must still get
# the generic op's answer. Each routine is called twice so the second call
# runs the compiled chunk.
class AttrSite { has $!a; has int $!n; has str $!s; has @!items; }
my sub as-get($o) { nqp::getattr($o, AttrSite, '$!a') }
my sub as-get-i($o, $d) { nqp::getattr_i($o, AttrSite, '$!n') }
my sub as-get-s($o, $d) { nqp::getattr_s($o, AttrSite, '$!a') }
my sub as-get-list($o, $d) { nqp::elems(nqp::getattr($o, AttrSite, '@!items')) }
my sub as-get-missing($o, $d) { nqp::getattr($o, AttrSite, '$!nope').raku }
my sub as-bind($o, $v) { nqp::bindattr($o, AttrSite, '$!a', $v) }
my sub as-bind-i($o, $v) { nqp::bindattr_i($o, AttrSite, '$!n', $v) }
my sub as-bind-s($o, $v) { nqp::bindattr_s($o, AttrSite, '$!s', $v) }
my sub as-bind-empty($o, $v) { nqp::bindattr($o, AttrSite, '$!', $v) }
my sub as-reified($r, $d) { nqp::elems(nqp::getattr($r, List, '$!reified')) }
my sub as-storage($h, $d) { nqp::elems(nqp::getattr($h, Map, '$!storage')) }
my sub as-rebind($r, $b) { nqp::bindattr($r, List, '$!reified', $b); nqp::elems($r) }
my sub as-match-pos($m, $d) { nqp::getattr_i($m, Match, '$!pos') }
{
    my $o = AttrSite.new;
    my @got;
    for ^2 {
        @got.push: as-get($o).raku;
        @got.push: as-bind($o, 41 + $_);
        @got.push: as-get($o);
        @got.push: as-bind-i($o, "7");
        @got.push: as-get-i($o, 0);
        @got.push: as-bind-s($o, 12);
        @got.push: as-get-s($o, 0);
        @got.push: as-get-list($o, 0);
        @got.push: as-get-missing($o, 0);
        @got.push: (try as-bind-empty($o, 1)) // $!.message;
        my @r = 1, 2, 3;
        my %h = a => 1, b => 2;
        @got.push: as-reified(@r, 0);
        @got.push: as-storage(%h, 0);
        @got.push: as-rebind(@r, nqp::list(9, 8));
        @got.push: as-match-pos("abcd" ~~ /bc/, 0);
    }
    say "attr-sites={@got.join(',')}";
}

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
