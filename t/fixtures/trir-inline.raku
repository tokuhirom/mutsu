# Fixture for t/vm/codegen/adr8673-trir-inline.t: small TRIR callees that are
# inlined into their TRIR callers (ADR-8673 D2), one per shape the splice has
# to keep: each prints what the caller saw. The run must be identical with
# inlining on, off (MUTSU_TRIR_INLINE=0) and with TRIR off.
use nqp;

# --- a native `is rw` parameter: the caller's own variable ------------------
sub bump(int $p is rw --> Nil) { $p = nqp::add_i($p, 1) }
sub bump-three(int $n) { my int $p = $n; bump($p); bump($p); bump($p); $p }
say "rw=", bump-three(10);

# --- an `is rw` parameter passed on: the reference the caller holds --------
sub bump-via(int $p is rw --> Nil) { bump($p); bump($p) }
sub use-via(int $n) { my int $p = $n; bump-via($p); $p }
say "rw-ref=", use-via(5);

# --- evaluated arguments, several of each bank ----------------------------
sub mixup(int $a, str $b, int $c, str $d) {
    nqp::add_i(nqp::mul_i($a, 100), nqp::add_i(nqp::mul_i($c, 10), nqp::chars(nqp::concat($b, $d))))
}
sub use-mix(int $n) { mixup(nqp::mul_i($n, 2), 'b', nqp::add_i($n, 1), 'dd') }
say "values=", use-mix(4);

# --- a read-only parameter written in the callee is a copy -----------------
sub shadow(int $x) { my int $y = $x; $y = nqp::add_i($y, 100); $y }
sub use-shadow(int $n) { shadow($n) }
say "copy=", use-shadow(1);

# --- the callee's free variable, rebound by a call it makes ----------------
my $table := nqp::list_i(10, 20, 30);
sub swap-table($x) { $table := nqp::list_i(70, 80, 90); $x }
sub peek-swap-peek(int $i) {
    my int $a = nqp::atpos_i($table, $i);
    swap-table(1);
    nqp::add_i($a, nqp::atpos_i($table, $i))
}
sub use-peek(int $i) { peek-swap-peek($i) }
say "outer=", use-peek(1), " then ", use-peek(2);

# --- a `fail` in the callee is its Failure value --------------------------
sub might-fail(int $n) { $n > 0 ?? $n !! fail("nonpositive $n") }
sub use-fail(int $n) { might-fail($n) }
sub show($r) { $r.defined ?? "ok $r" !! "failed: {$r.exception.message}" }
say "fail=", show(use-fail(3)), " / ", show(use-fail(-1));

# --- an exception (not a fail) leaves through the caller ------------------
sub boom(int $n) { $n > 5 ?? die("too big $n") !! $n }
sub use-boom(int $n) { boom($n) }
say "die=", (try use-boom(9)) // "caught: {$!.message}", " ", use-boom(2);

# --- the inlined callee is on the stack for what it calls -----------------
sub where-am-i($x) { callframe(1).code.name ~ "<" ~ callframe(2).code.name }
sub inner-site($x) { where-am-i($x) }
sub outer-site($x) { inner-site($x) }
say "frames=", outer-site(1);

# --- a callee replaced by `.wrap` after the caller linked it ---------------
sub small(int $n) { nqp::add_i($n, 1) }
sub use-small(int $n) { small($n) }
say "before-wrap=", use-small(1), ",", use-small(2);
&small.wrap(sub ($n) { callsame() * 10 });
say "after-wrap=", use-small(1), ",", use-small(2);

# --- a callee that returns early from inside a loop ------------------------
sub first-space(str $s --> int) {
    my int $i = 0;
    my int $n = nqp::chars($s);
    nqp::while(nqp::islt_i($i, $n), nqp::stmts(
        nqp::if(nqp::iseq_i(nqp::ordat($s, $i), 32), (return $i)),
        ($i = nqp::add_i($i, 1))));
    -1
}
sub use-first-space(str $s) { first-space($s) }
say "early-return=", use-first-space("ab cd"), ",", use-first-space("abcd"), ",", use-first-space(" x");
