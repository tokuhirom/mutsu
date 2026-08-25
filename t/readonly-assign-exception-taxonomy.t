use Test;

# Rakudo splits "you cannot assign to this" into three distinct exceptions, and
# which one you get is a property of the LVALUE, not of the assignment site:
#
#   1. a readonly *binding* that still owns a container (a non-`is rw`
#      parameter, a `for` loop's named alias)
#         -> X::AdHoc, "Cannot assign to a readonly variable or a value"
#   2. a sigiled *variable* with no container at all, bound straight to an
#      immutable value (`my $x := 42`, `my constant $PI`, a topic aliased to a
#      literal)
#         -> X::AdHoc, "Cannot assign to an immutable value"
#   3. a name that denotes the immutable *value* itself (a sigilless
#      `constant PI` / `\c` term, a literal, an immutable container)
#         -> X::Assignment::RO, "Cannot modify an immutable TYPE (VALUE)"
#
# mutsu used to route all three through X::Assignment::RO, so
# `CATCH { when X::AdHoc { ... } }` around a `for %h.values -> $v { $v = ... }`
# typo did not catch what it catches in Rakudo.

# ---------------------------------------------------------------------------
# 1. Readonly binding with a container: X::AdHoc / "readonly variable"
# ---------------------------------------------------------------------------

throws-like { sub f($x) { $x = 10 }; f(5) }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'assigning to a non-rw sub parameter';

throws-like { sub f($x) { $x += 1 }; f(5) }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'compound-assigning to a non-rw sub parameter';

throws-like { sub f(:$x) { $x = 1 }; f(x => 2) }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'assigning to a non-rw named parameter';

throws-like { class C { method m($x) { $x = 5 } }; C.new.m(3) }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'assigning to a non-rw method parameter';

throws-like { my %h = a => 23; for %h.values -> $v { $v += 10 } }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'assigning to a for-loop alias over %h.values';

throws-like { my %h = a => 23; for %h.keys -> $k { $k = 'z' } }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'assigning to a for-loop alias over %h.keys';

throws-like { my @a = 1, 2, 3; for @a -> $v { $v = 9 } }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'assigning to a for-loop alias over an Array (the alias is readonly)';

throws-like { for (1, 2, 3) -> $v { $v = 9 } }, X::AdHoc,
    message => /'Cannot assign to a readonly variable or a value'/,
    'assigning to a for-loop alias over a literal list';

# ---------------------------------------------------------------------------
# 2. No container at all: X::AdHoc / "immutable value"
# ---------------------------------------------------------------------------

throws-like { my $x := 42; $x = 23 }, X::AdHoc,
    message => /'Cannot assign to an immutable value'/,
    'assigning to a := -bound Int literal';

throws-like { my $x := 'abc'; $x = 'd' }, X::AdHoc,
    message => /'Cannot assign to an immutable value'/,
    'assigning to a := -bound Str literal';

throws-like { my $x := 5; $x += 1 }, X::AdHoc,
    message => /'Cannot assign to an immutable value'/,
    'compound-assigning to a := -bound literal';

throws-like { my constant $PI = 3.14; $PI = 5 }, X::AdHoc,
    message => /'Cannot assign to an immutable value'/,
    'assigning to a SIGILED constant (a variable with no container)';

throws-like { given 5 { $_ = 6 } }, X::AdHoc,
    message => /'Cannot assign to an immutable value'/,
    'assigning to a topic aliased to a literal';

throws-like { given 5 { when Int { $_ = 6 } } }, X::AdHoc,
    message => /'Cannot assign to an immutable value'/,
    'assigning to a when-block topic aliased to a literal';

throws-like { my @a = 1, 2; given @a { $_ = 5 } }, X::AdHoc,
    message => /'Cannot assign to an immutable value'/,
    'whole-reassigning a container topic';

throws-like { for 1 .. 2 { $_ = 5 } }, X::AdHoc,
    message => /'Cannot assign to an immutable value'/,
    'assigning to a topic aliased to a Range element';

# A Proxy RETURNED FROM A SUB is FETCHed by the return, so the bind sees the
# fetched (immutable) value, not the Proxy -- hence the same "immutable value".
throws-like {
    sub double() {
        my $s = 0;
        Proxy.new(FETCH => method () { $s * 2 }, STORE => method ($n) { $s = $n })
    }
    my $doubled := double();
    $doubled = 4;
}, X::AdHoc,
    message => /'Cannot assign to an immutable value'/,
    'assigning through a := -bound Proxy returned from a sub call';

# ---------------------------------------------------------------------------
# 3. The name IS an immutable value: X::Assignment::RO
# ---------------------------------------------------------------------------

throws-like { my constant PI = 3.14; PI = 5 }, X::Assignment::RO,
    message => /'Cannot modify an immutable Rat (3.14)'/,
    'assigning to a SIGILLESS constant term';

throws-like { my \c = 5; c = 6 }, X::Assignment::RO,
    message => /'Cannot modify an immutable Int (5)'/,
    'assigning to a sigilless bound term';

throws-like { 1 = 2 }, X::Assignment::RO,
    message => /'Cannot modify an immutable Int (1)'/,
    'assigning to an Int literal';

throws-like { 'a' = 'b' }, X::Assignment::RO,
    message => /'Cannot modify an immutable Str (a)'/,
    'assigning to a Str literal';

# The exact wording of these two is still approximate in mutsu, so only the
# class is pinned here.
throws-like { my %m := mix <a b>; %m = (c => 1) }, X::Assignment::RO,
    'whole-reassigning a := -bound immutable Mix';

throws-like { my constant @A = 1, 2, 3; @A = 5 }, X::Assignment::RO,
    'whole-reassigning a sigilless-style constant Array';

# ---------------------------------------------------------------------------
# Writable controls: none of the above may broaden into these.
# ---------------------------------------------------------------------------

{
    my @a = 1, 2, 3;
    for @a { $_ = $_ * 10 }
    is-deeply @a, [10, 20, 30], 'for @a { $_ = ... } still mutates the elements';
}

{
    my %h = a => 1, b => 2;
    for %h.values { $_ += 10 }
    is-deeply %h.values.sort.list, (11, 12), 'for %h.values { $_ = ... } still mutates';
}

{
    my $r = do { sub f($x is copy) { $x = 10; $x }; f(5) };
    is $r, 10, 'an "is copy" parameter is writable';
}

{
    my $v = 1;
    sub g($x is rw) { $x = 7 }
    g($v);
    is $v, 7, 'an "is rw" parameter writes back';
}

{
    my @a = 1, 2, 3;
    @a[0] = 9;
    is @a[0], 9, 'an Array element is writable';
}

{
    my $src = 1;
    my $alias := $src;
    $alias = 23;
    is $src, 23, 'binding to a writable variable stays writable';
}

{
    my $x = 5;
    given $x { $_ = 6 }
    is $x, 6, 'a topic aliased to a writable variable stays writable';
}

{
    my $s = 0;
    my $p := Proxy.new(FETCH => method () { $s * 2 }, STORE => method ($n) { $s = $n });
    $p = 4;
    is $p, 8, 'binding a Proxy directly (no sub call) stays writable';
}

# ---------------------------------------------------------------------------
# .VAR reflects the same container/no-container split.
# ---------------------------------------------------------------------------

{
    my $a = 1;
    is $a.VAR.^name, 'Scalar', 'plain assignment creates a Scalar container';
    my $b := 1;
    is $b.VAR.^name, 'Int', 'a := -bound literal has NO container';
    my constant PI2 = 3.14;
    is PI2.VAR.^name, 'Rat', 'a sigilless constant term has NO container';
    my constant $E = 2;
    is $E.VAR.^name, 'Int', 'a sigiled constant has NO container';
}

{
    sub h($x) { $x.VAR.^name }
    is h(5), 'Scalar', 'a readonly parameter DOES have a (readonly) container';
    my @a = 1, 2;
    my $n;
    for @a -> $v { $n = $v.VAR.^name; last }
    is $n, 'Scalar', 'a for-loop alias DOES have a (readonly) container';
}

done-testing;
