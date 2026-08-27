use Test;

# Regression test for the fixed rows of
# todo/tickets/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md: cases
# where Raku rejects an assignment to an immutable lvalue but mutsu used to
# silently succeed. Every shape below was verified against `raku` v2026.06
# before this test was written.

# ---------------------------------------------------------------------------
# 1. A plain `$`-sigiled pointy-block parameter is a readonly ALIAS (like a
#    non-`is rw` sub parameter), regardless of how it is invoked.
# ---------------------------------------------------------------------------

throws-like { my $b = -> $v { $v = 1 }; $b(3) }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'assigning to a single-param pointy-block parameter';

throws-like { my $b = -> $v { $v += 1 }; $b(3) }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'compound-assigning to a single-param pointy-block parameter';

throws-like { my $b = -> $v, $w { $v = 1 }; $b(3, 4) }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'assigning to a multi-param pointy-block parameter (already worked; pinned here too)';

# ---------------------------------------------------------------------------
# 2. Assigning to a bareword that IS the immutable value (a builtin type
#    object, the bare `Nil` term, or an enum value) -- X::Assignment::RO.
# ---------------------------------------------------------------------------

throws-like { Int = 5 }, X::Assignment::RO,
    message => /'Cannot modify an immutable \'Int\' type object'/,
    'assigning to the Int type object';

throws-like { Nil = 5 }, X::Assignment::RO,
    message => /'Cannot modify an immutable Nil value'/,
    'assigning to the bare Nil term';

throws-like { enum Fo <A B>; A = 3 }, X::Assignment::RO,
    message => /'Cannot modify an immutable Fo (A)'/,
    'assigning to an enum value';

throws-like { class Foo {}; Foo = 5 }, X::Assignment::RO,
    message => /'Cannot modify an immutable \'Foo\' type object'/,
    'assigning to a user-declared class type object (also fixes its exception class)';

# `constant` shadowing a builtin type name is a DECLARATION, not a
# modification, and must stay legal.
{
    constant Int = 5;
    is Int, 5, 'a constant may shadow a builtin type name';
}

# ---------------------------------------------------------------------------
# 3. `my @a := (1,2,3); @a.push(...)` -- an Array-sigiled variable bound to an
#    immutable List, the array twin of the already-working scalar bind
#    (`my $a := (1,2,3); $a.push(...)`).
# ---------------------------------------------------------------------------

# `splice` is deliberately excluded: Raku does not define a `splice`
# candidate on a plain `List` at all (X::Multi::NoMatch, "Routine does not
# have any candidates"), so it never reaches the immutable-container check
# these methods do -- that is a separate, pre-existing gap, not one of the
# rows this test pins.
for <push append unshift prepend pop shift> -> $method {
    my $code = do given $method {
        when 'push'     { -> @a { @a.push(4) } }
        when 'append'   { -> @a { @a.append(4) } }
        when 'unshift'  { -> @a { @a.unshift(0) } }
        when 'prepend'  { -> @a { @a.prepend(0) } }
        when 'pop'      { -> @a { @a.pop } }
        when 'shift'    { -> @a { @a.shift } }
    };
    throws-like { my @a := (1, 2, 3); $code(@a) }, X::Immutable,
        message => /'immutable' .* 'List'/,
        ":= -bound Array rejects .$method";
}

# ---------------------------------------------------------------------------
# 4. Sub-signature destructuring: neither a sigilless leaf (`\a`) nor a plain
#    `$`-sigiled leaf inside a `(...)` sub-signature was ever marked readonly.
# ---------------------------------------------------------------------------

throws-like { sub f($ (\a, \b)) { a = 1 }; f((10, 20)) }, X::Assignment::RO,
    message => /'Cannot modify an immutable Int (10)'/,
    'assigning to a sigilless sub-signature destructure leaf';

throws-like { sub f($ ($x, $y)) { $x = 1 }; f((10, 20)) }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'assigning to a $-sigiled sub-signature destructure leaf';

# ---------------------------------------------------------------------------
# 5. `my \G = 5; G++` -- postfix/prefix `++`/`--` on a sigilless bind now
#    dispatches the same way an in-place mutation of a readonly parameter does.
# ---------------------------------------------------------------------------

throws-like { my \G = 5; G++ }, X::Multi::NoMatch,
    'postfix ++ on a sigilless bind';

throws-like { my \G = 5; G-- }, X::Multi::NoMatch,
    'postfix -- on a sigilless bind';

throws-like { my \G = 5; ++G }, X::Multi::NoMatch,
    'prefix ++ on a sigilless bind';

# ---------------------------------------------------------------------------
# Writable controls: none of the fixes above may broaden into these.
# ---------------------------------------------------------------------------

{
    my $r = do { sub f($x is rw) { $x = 1 }; my $v = 5; f($v); $v };
    is $r, 1, 'a sub "is rw" parameter still writes back';
}

{
    my $r = do { my $b = -> $v is rw { $v = 1 }; my $v = 5; $b($v); $v };
    is $r, 1, 'a pointy-block "is rw" parameter still writes back';
}

{
    my $r = do { my $b = -> $v is copy { $v = 1; $v }; $b(3) };
    is $r, 1, 'a pointy-block "is copy" parameter is still writable';
}

{
    my @a = 1, 2, 3;
    for @a { $_ = $_ * 10 }
    is-deeply @a, [10, 20, 30], 'for @a { $_ = ... } still mutates a real Array\'s elements';
}

{
    my @a = 1, 2, 3;
    @a.push(4);
    is-deeply @a, [1, 2, 3, 4], 'push on a real (non-bound) Array still works';
}

{
    my @a := my @b = 1, 2, 3;
    @a.push(4);
    is-deeply @b, [1, 2, 3, 4], 'binding an @-var to a real Array keeps push writable';
}

{
    my $r = do {
        my $x = 0;
        my @a = 1, 2, 3;
        @a.map({ $_ *= 10 });
        @a;
    };
    is-deeply $r, [10, 20, 30], '@a.map({ $_ = ... }) topic mutation over a real Array still writes back';
}

# --- regression controls for two bugs the fix above ran into in CI ---

{
    # A `for`-loop sub-signature destructure leaf (`-> ($str, $expected,
    # |args)`, roast S32-str/comb.t) binds by a direct `SetGlobal`, not a
    # local slot. Its first-ever write must not be misidentified as
    # assigning to the lowercase native-type synonym of the same bare name
    # (`str`, `int`, `num`, `array`, `bool`, ...).
    my @tests = ("abc", <a b c>.Seq, ""), ("xyz", <x y z>.Seq, "", 4);
    my @seen;
    for @tests -> ($str, $expected, |args) {
        @seen.push($str);
    }
    is-deeply @seen, ["abc", "xyz"],
        'for-loop sub-signature destructure leaf named like a lowercase native type stays writable';
}

{
    # An uninitialized outer `my $str;` captured by a closure and assigned
    # INSIDE the closure (`lives-ok { $str = 1 }, "..."`) must stay writable.
    # `SetVarDynamic`'s closure-capture-by-reference support pre-seeds a
    # not-yet-assigned captured variable's env slot with the placeholder
    # `Package(Any)` regardless of the variable's own name, so this hit the
    # exact same lowercase-native-type collision as the for-loop destructure
    # leaf above, but via a different current-value shape (`Package(Any)`
    # instead of `None`). `lives-ok` catches the resulting spurious
    # X::Assignment::RO and reports a false test failure -- the outer `plan`
    # below asserts the assignment itself is silently accepted, i.e. that
    # `lives-ok`'s own inner test reports "ok".
    my $str;
    my $inner_ok = lives-ok { $str = 1 }, "assigning to a captured, uninitialized \$str";
    ok $inner_ok, 'lives-ok on a captured uninitialized $str reports success (roast S02-types/set.t, sethash.t)';
    is $str, 1, 'and the assignment actually took effect';
}

{
    # The reduced repro that first caught this in CI, pinned verbatim.
    my $str;
    my $inner_ok = lives-ok { $str = 1 }, "x";
    ok $inner_ok, 'reduced repro: my $str; lives-ok { $str = 1 } reports success';
}

{
    # Same class of bug, checked against dies-ok/throws-like/plain try too --
    # a spurious X::Assignment::RO would show up in all of them, since they
    # all inspect the block's thrown-or-not outcome the same way lives-ok does.
    my $str;
    todo "dies-ok correctly reports false: the assignment is legal and does not die";
    my $inner_ok = dies-ok { $str = 1 }, "should NOT die (assignment is legal)";
    nok $inner_ok, 'dies-ok on a captured uninitialized $str correctly reports "did not die"';
    is $str, 1, 'and the assignment still took effect';
}

{
    my $str;
    throws-like { $str = 1; die "boom" }, X::AdHoc,
        message => /'boom'/,
        'throws-like still sees the REAL exception, not a spurious one from the assignment';
    is $str, 1, 'and the assignment before the real die still took effect';
}

{
    my $str;
    my $threw = False;
    try {
        $str = 1;
        CATCH { default { $threw = True } }
    }
    nok $threw, 'a plain try{} around the same assignment does not set $!/CATCH';
    is $str, 1, 'and the assignment took effect';
}

{
    # A plain single-param pointy block reused across an outer `for ^N {}`
    # AND a later `for ... -> ($x, $y) {}` sharing a variable name (the
    # digest-battery.t / SHA3 shape) must not leak a readonly mark from the
    # first pointy-block call into the later destructure.
    my $r = do {
        sub f() {
            my @lanes = [1, 2, 3, 4, 5] xx 5;
            my @c = map -> $x { [+^] @lanes[$x; ^5] }, ^5;
            for ^2 X ^2 -> ($x, $y) { @lanes[$x; $y] +^= @c[$x] }
            @lanes;
        }
        f();
    };
    ok $r.elems == 5, 'pointy-block param mark does not leak into a later for-loop destructure of the same name';
}

{
    # The native `.map` rw-writeback path (`next`/`last` inside an `is rw`
    # param block) must still work after moving the plain pointy-block
    # readonly mark to the call site.
    my @a = 1, 2, 3, 4, 5;
    my @r = @a.map(-> $x is rw { next if $x %% 2; $x++; $x });
    is-deeply @a, [2, 2, 4, 4, 6], 'next inside an rw-param native map block still skips its own mutation';
    is-deeply @r, [2, 4, 6], 'and the return value still omits the skipped iterations';
}

{
    # `$_ = $state` where `$state` holds an enum value from an earlier loop
    # iteration must not be treated as reassigning that enum member itself
    # (t/topic-alias-does-not-cross-frames.t).
    my enum State <A B>;
    sub advance() {
        my $state = A;
        loop {
            $_ = $state;
            when A { $state = B; next }
            when B { last }
        }
        1;
    }
    class C { method tag() { 'C' } }
    my $seen;
    given C.new -> $c {
        advance();
        $seen = $c.tag;
    }
    is $seen, 'C', 'a when-loop driven off its topic does not corrupt an outer given-binding';
}

done-testing;
