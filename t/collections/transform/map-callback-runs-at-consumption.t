use Test;

# rakudo's `.map`/`.grep` return a lazy `Seq`: the callback runs when something
# consumes the sequence, not at the `.map` call. Every assertion in this file
# was verified by running this exact file under real `raku`, which passes it
# 37/37 --- there are no `todo` rows left.
#
# The escaping rows are checked in a subprocess because they kill the program:
# a statement-position `try { ... }` leaves its tail value un-sunk (rakudo's
# `try` handler wrapper stops sink propagation), so the *enclosing* statement
# sinks it OUTSIDE the try's protection, and a callback that throws while that
# sink forces the Seq is uncaught. `t/try-sink-semantics.t` pins the
# sink-placement half of this; the laziness half is docs/adr/0058.
#
# Part 2's rows were `todo` until ADR-0058 step 2 landed: mutsu used to
# evaluate a `.map` over a finite source eagerly, at the `.map` call, so the
# callback had already thrown by the time the `try` block's tail value existed.
# `.map` now hands back a `Seq` whose `SeqSource::MapGrep` body runs the
# callback at first consumption, so these rows pass.
#
# Part 1's two `todo`s (a force-time `fail` under an enclosing `try` answering
# a Failure instead of throwing) went with ADR-0058 S9.4: `try` implies
# `use fatal`, `SeqSource::MapGrep` captures that at the `.map` CALL, and the
# pull now turns a `Control::Fail` raised under it into a hard throw instead of
# leaving the next routine boundary to soften it into a returned Failure.
#
# Part 3 is the SPELLING audit S9.4 asked for. Every Part 1/2 row above has a
# List or Range receiver, and that is exactly why they all passed while
# `@a.map({ ... })` --- a `.map` on a real `@` array, the commonest spelling
# there is --- stayed eager: it compiles to `OpCode::CallMethodMut` and never
# reached the deferral. Part 3 repeats the shapes with that receiver.

plan 37;

sub run-snippet($code) {
    my $p = run($*EXECUTABLE, '-e', $code, :out, :err);
    my $out = $p.out.slurp(:close);
    $p.err.slurp(:close);
    ($p.exitcode, $out)
}

# ---------------------------------------------------------------------------
# Part 1 --- a `...` stub callback. `...` IS a `fail`, so these rows are also
# what pins the `use fatal` captured at the `.map` call (ADR-0058 S9.4).
# ---------------------------------------------------------------------------

{
    my ($rc, $out) = run-snippet(
        'sub ee { try { map -> $x, $y { ... }, 1..6 }; say "reached-tail"; $! }; say ee().^name; say "alive"');
    isnt $rc, 0, 'Q5/R6: stub-map under a sub-scope try escapes the try';
    unlike $out, /'reached-tail'/, 'Q5/R6: ... and the statement after the try never runs';
}
{
    my ($rc, $out) = run-snippet(
        'sub f { map -> $x, $y { ... }, 1..6 }; sub ee { try { f() }; say "reached-tail"; $! }; say ee().^name; say "alive"');
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
    isnt $rc, 0, 'P4: a dying map callback escapes a statement-position try';
    unlike $out, /'alive'/, 'P4: ... and the next statement never runs';
}
{
    my ($rc, $out) = run-snippet('sub f { (1..3).map({die "boom"}) }; try { f() }; say "alive"');
    isnt $rc, 0, 'P5: same, through one level of call indirection';
    unlike $out, /'alive'/, 'P5: ... and the next statement never runs';
}
{
    my ($rc, $out) = run-snippet(
        'sub f { (1..3).map({die "boom"}) }; sub ee { try { f() } }; say ee().^name; say "alive"');
    is $rc, 0, 'P18: the program survives a dying map Seq used as the try value';
    like $out, /^^ 'Seq'/, 'P18: ... and the unforced Seq is still a Seq';
}
{
    my ($rc, $out) = run-snippet(
        'try { (1..3).map({die "boom"}) }; CATCH { default { say "unit-caught" } }; say "alive"');
    is $rc, 0, 'Q9: the program survives an escape caught by the enclosing CATCH';
    like $out, /'unit-caught'/, 'Q9: ... and the enclosing block CATCH reports it';
}
{
    my ($rc, $out) = run-snippet('sub f { (1..3).map({ fail "x" }) }; try { f() }; say "alive"');
    isnt $rc, 0, 'Q14: a failing map callback escapes the try too';
    unlike $out, /'alive'/, 'Q14: ... and the next statement never runs';
}
{
    my ($rc, $out) = run-snippet(
        'my $s = (1..3).map({ say "side $_"; $_ }); say "before"; say $s.List');
    like $out, /^ 'before'/, 'the map callback runs after the statement following the .map';
}

# ---------------------------------------------------------------------------
# Part 3 --- the same shapes with a REAL `@` array receiver (ADR-0058 S9.4).
# ---------------------------------------------------------------------------

{
    my ($rc, $out) = run-snippet(
        'my @a = 1,2,3; try { @a.map({die "boom"}) }; say "alive ", $!.defined');
    isnt $rc, 0, 'P4/@a: a dying map callback on an @ array escapes a statement-position try';
    unlike $out, /'alive'/, 'P4/@a: ... and the next statement never runs';
}
{
    my ($rc, $out) = run-snippet(
        'my @a = 1,2,3; sub f { @a.map({die "boom"}) }; try { f() }; say "alive"');
    isnt $rc, 0, 'P5/@a: same, through one level of call indirection';
    unlike $out, /'alive'/, 'P5/@a: ... and the next statement never runs';
}
{
    my ($rc, $out) = run-snippet(
        'my @a = 1,2,3; sub f { @a.map({die "boom"}) }; sub ee { try { f() } }; say ee().^name; say "alive"');
    is $rc, 0, 'P18/@a: the program survives a dying map Seq used as the try value';
    like $out, /^^ 'Seq'/, 'P18/@a: ... and the unforced Seq is still a Seq';
}
{
    my ($rc, $out) = run-snippet(
        'my @a = 1,2,3; sub f { @a.map({ fail "x" }) }; try { f() }; say "alive"');
    isnt $rc, 0, 'Q14/@a: a failing map callback on an @ array escapes the try too';
    unlike $out, /'alive'/, 'Q14/@a: ... and the next statement never runs';
}
{
    my ($rc, $out) = run-snippet(
        'my @a = 1,2,3; my $s = @a.map({ say "side $_"; $_ }); say "before"; say $s.List');
    like $out, /^ 'before'/, '@a: the map callback runs after the statement following the .map';
}
{
    # The rw-writeback spelling: rakudo writes the mutated elements back at
    # CONSUMPTION, not at the `.map` call, so `@a` is untouched in between.
    my ($rc, $out) = run-snippet(
        'my @a = 1,2,3; my $s = @a.map({ $_++; $_ }); say "before ", @a; say $s.List; say "after ", @a');
    is $rc, 0, '@a: an rw map over an @ array runs';
    like $out, /^ 'before [1 2 3]'/, '@a: ... and the rw write-back has not happened yet';
    like $out, /'after [2 3 4]'/, '@a: ... but it has by the time the Seq is consumed';
}
{
    # The listop form over an `@` array (`builtin_map`'s own rw path).
    my ($rc, $out) = run-snippet(
        'my @a = 1..6; my $r = (map -> $x, $y { ... }, @a); say "made-it"; say $r.List');
    isnt $rc, 0, '@a listop: consuming a stub-map Seq over an @ array runs the stub and throws';
    like $out, /'made-it'/, '@a listop: ... only after the statements before the consumption';
}
