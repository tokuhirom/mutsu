use Test;

# Regression: wrapping a sub with a closure wrapper must not let the wrapper's
# stale captured env snapshot clobber live caller lexicals on return, and must
# still propagate the wrapper's genuine mutations of captured variables.
#
# Root cause (fixed): the interpreter's call_sub_value overlaid and wrote back a
# sub's captured-env snapshot wholesale. The original sub, called via callsame,
# carried a stale snapshot of `$h` (captured as Nil mid-`my $h = &foo.wrap(...)`)
# and reverted the live WrapHandle; symmetrically, scalar mutations a wrapper made
# to captured outer variables were lost. Both directions are exercised here.

plan 8;

# 1-2. The WrapHandle variable survives invoking the wrapped sub, and .restore works.
{
    sub qux { 42 }
    my $h = &qux.wrap(-> { 1 + callsame });
    is qux(), 43, 'wrapped call runs the wrapper';
    $h.restore();
    is qux(), 42, '$h is not clobbered by the wrapped call; .restore() works';
}

# 3. A wrapper's scalar mutation of a captured variable propagates to the caller.
{
    my $seen = False;
    sub f1 { 1 }
    &f1.wrap(-> { $seen = True; callsame });
    f1();
    is $seen, True, 'wrapper scalar mutation of captured var propagates';
}

# 4. Counter accumulates across multiple wrapped calls.
{
    my $n = 0;
    sub f2 { 1 }
    &f2.wrap(-> { $n++; callsame });
    f2();
    f2();
    is $n, 2, 'wrapper increments a captured counter across calls';
}

# 5. Array mutation by a wrapper is visible to the caller.
{
    my @log;
    sub f3 { 1 }
    &f3.wrap(-> { @log.push("x"); callsame });
    f3();
    is @log.join(','), 'x', 'wrapper array mutation propagates';
}

# 6. An unrelated caller lexical is untouched by a wrapped call.
{
    sub f4 { 1 }
    my $other = "keep";
    &f4.wrap(-> { callsame });
    f4();
    is $other, "keep", 'unrelated caller lexical not clobbered';
}

# 7. Named-sub wrapper still works (no closure capture path).
{
    my $seen2 = False;
    sub f5 { 1 }
    sub w5 { $seen2 = True; callsame }
    &f5.wrap(&w5);
    f5();
    is $seen2, True, 'named-sub wrapper mutation propagates';
}

# 8. Return value modification through callsame.
{
    sub greeting { "hello" }
    &greeting.wrap(sub { callsame() ~ " world" });
    is greeting(), "hello world", 'wrapper can modify return value';
}
