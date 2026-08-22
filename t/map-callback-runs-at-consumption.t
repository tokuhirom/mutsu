use Test;

# rakudo's `.map`/`.grep` return a lazy `Seq`: the callback runs when something
# consumes the sequence, not at the `.map` call. Every assertion in this file
# was verified by running this exact file under real `raku`, which passes it
# 23/23 (the `todo` rows pass there --- they are what mutsu is aiming at).
#
# The escaping rows are checked in a subprocess because they kill the program:
# a statement-position `try { ... }` leaves its tail value un-sunk (rakudo's
# `try` handler wrapper stops sink propagation), so the *enclosing* statement
# sinks it OUTSIDE the try's protection, and a callback that throws while that
# sink forces the Seq is uncaught. `t/try-sink-semantics.t` pins the
# sink-placement half of this; the laziness half is docs/adr/0058.
#
# Part 2's rows are `todo` because mutsu evaluates a `.map` over a finite
# source eagerly, at the `.map` call, so the callback has already thrown by the
# time the `try` block's tail value exists. Un-`todo` them when ADR-0058 lands
# --- they are that ADR's completion oracle.

plan 23;

sub run-snippet($code) {
    my $p = run($*EXECUTABLE, '-e', $code, :out, :err);
    my $out = $p.out.slurp(:close);
    $p.err.slurp(:close);
    ($p.exitcode, $out)
}

# ---------------------------------------------------------------------------
# Part 1 --- a `...` stub callback, which mutsu already defers
# (`create_lazy_map_list`, gated on `is_stub_routine_body`).
# ---------------------------------------------------------------------------

{
    my ($rc, $out) = run-snippet(
        'sub ee { try { map -> $x, $y { ... }, 1..6 }; say "reached-tail"; $! }; say ee().^name; say "alive"');
    todo 'a force-time `fail` under an enclosing `try` returns a Failure instead of throwing';
    isnt $rc, 0, 'Q5/R6: stub-map under a sub-scope try escapes the try';
    unlike $out, /'reached-tail'/, 'Q5/R6: ... and the statement after the try never runs';
}
{
    my ($rc, $out) = run-snippet(
        'sub f { map -> $x, $y { ... }, 1..6 }; sub ee { try { f() }; say "reached-tail"; $! }; say ee().^name; say "alive"');
    todo 'a force-time `fail` under an enclosing `try` returns a Failure instead of throwing';
    isnt $rc, 0, 'Q6/R7: call-returned stub-map under a sub-scope try escapes too';
    unlike $out, /'reached-tail'/, 'Q6/R7: ... and the statement after the try never runs';
}
{
    my ($rc, $out) = run-snippet(
        'sub f { map -> $x, $y { ... }, 1..6 }; sub ee { try { f() } }; say ee().^name; say "alive"');
    is $rc, 0, 'a stub-map Seq returned as the try value is never forced';
    like $out, /'alive'/, '... and the program runs on';
}
{
    my ($rc, $out) = run-snippet(
        'sub ee { my $r = try { map -> $x, $y { ... }, 1..6 }; say "r=", $r.^name; $! }; say ee().^name; say "alive"');
    is $rc, 0, 'assigning a stub-map Seq to a scalar is not sink context';
    like $out, /'r=Seq'/, '... and it is still a Seq';
}
{
    my ($rc, $out) = run-snippet(
        'my $r = (map -> $x, $y { ... }, 1..6); say "made-it"; say $r.List');
    isnt $rc, 0, 'consuming a stub-map Seq runs the stub and throws';
    like $out, /'made-it'/, '... only after the statements before the consumption';
}
{
    # `for $r` would NOT force it: a `$`-contained Seq iterates as one item in
    # rakudo, so the loop is written over the map expression itself.
    my ($rc, $out) = run-snippet(
        'say "made-it"; for (map -> $x, $y { ... }, 1..6) { }; say "unreached"');
    isnt $rc, 0, 'a for loop over a stub-map Seq forces it';
    unlike $out, /'unreached'/, '... and never reaches the statement after the loop';
}

# ---------------------------------------------------------------------------
# Part 2 --- ADR-0058's target rows: an ordinary callback, which mutsu runs
# eagerly at the `.map` call.
# ---------------------------------------------------------------------------

{
    my ($rc, $out) = run-snippet('try { (1..3).map({die "boom"}) }; say "alive ", $!.defined');
    todo 'ADR-0058: a finite-source .map runs its callback eagerly', 2;
    isnt $rc, 0, 'P4: a dying map callback escapes a statement-position try';
    unlike $out, /'alive'/, 'P4: ... and the next statement never runs';
}
{
    my ($rc, $out) = run-snippet('sub f { (1..3).map({die "boom"}) }; try { f() }; say "alive"');
    todo 'ADR-0058: a finite-source .map runs its callback eagerly', 2;
    isnt $rc, 0, 'P5: same, through one level of call indirection';
    unlike $out, /'alive'/, 'P5: ... and the next statement never runs';
}
{
    my ($rc, $out) = run-snippet(
        'sub f { (1..3).map({die "boom"}) }; sub ee { try { f() } }; say ee().^name; say "alive"');
    is $rc, 0, 'P18: the program survives a dying map Seq used as the try value';
    todo 'ADR-0058: a finite-source .map runs its callback eagerly';
    like $out, /^^ 'Seq'/, 'P18: ... and the unforced Seq is still a Seq';
}
{
    my ($rc, $out) = run-snippet(
        'try { (1..3).map({die "boom"}) }; CATCH { default { say "unit-caught" } }; say "alive"');
    is $rc, 0, 'Q9: the program survives an escape caught by the enclosing CATCH';
    todo 'ADR-0058: a finite-source .map runs its callback eagerly';
    like $out, /'unit-caught'/, 'Q9: ... and the enclosing block CATCH reports it';
}
{
    my ($rc, $out) = run-snippet('sub f { (1..3).map({ fail "x" }) }; try { f() }; say "alive"');
    todo 'ADR-0058: a finite-source .map runs its callback eagerly', 2;
    isnt $rc, 0, 'Q14: a failing map callback escapes the try too';
    unlike $out, /'alive'/, 'Q14: ... and the next statement never runs';
}
{
    my ($rc, $out) = run-snippet(
        'my $s = (1..3).map({ say "side $_"; $_ }); say "before"; say $s.List');
    todo 'ADR-0058: a finite-source .map runs its callback eagerly';
    like $out, /^ 'before'/, 'the map callback runs after the statement following the .map';
}
