use Test;
plan 18;

# A sigilless parameter (`\p`) is implicitly *raw* in Raku: it binds the
# caller's container, not a value copy. mutsu used to leave `\p` out of every
# container-aliasing gate (`ParamDef::binds_caller_container` now unifies
# them), so it got only the by-name `__mutsu_sigilless_alias::p` bookkeeping —
# which reconciles the caller through a one-shot VALUE writeback at return.
# Any binding that OUTLIVES the call therefore never reached the caller.
#
# Every assertion below is raku's answer, verified with `raku` on this file.
#
# IMPORTANT: the outer variable is deliberately given a name DIFFERENT from the
# parameter's wherever the shape allows it. The blanket by-name splice this
# work replaced made several of these pass only when the two names coincided
# (which is why `roast/S32-list/tail.t`'s `PredictiveIterator` subtest passed
# on a broken mechanism), so a same-name test cannot tell a real fix from the
# coincidence.

# --- 1-2: bind through a raw param, written inside the same call ------------
{
    my $counter = 0;
    sub raw-same-call(\p) { my $x := p; $x = 1 }
    raw-same-call($counter);
    is $counter, 1, 'raw param: a := alias written in the same call reaches the caller';
}
{
    my $counter = 0;
    sub rw-same-call($p is rw) { my $x := $p; $x = 1 }
    rw-same-call($counter);
    is $counter, 1, 'is rw param: a := alias written in the same call reaches the caller';
}

# --- 3-4: the binding OUTLIVES the call (stored in an attribute) ------------
{
    my $counter = 0;
    my class RawHolder {
        has $!slot;
        method set(\p) { $!slot := p }
        method bump { $!slot++ }
    }
    my $h = RawHolder.new;
    $h.set($counter);
    $h.bump;
    is $counter, 1, 'raw param bound into an attribute still writes to the caller later';
}
{
    my $counter = 0;
    my class RwHolder {
        has $!slot;
        method set($p is rw) { $!slot := $p }
        method bump { $!slot++ }
    }
    my $h = RwHolder.new;
    $h.set($counter);
    $h.bump;
    is $counter, 1, 'is rw param bound into an attribute still writes to the caller later';
}

# --- 5: the binding outlives the call inside a returned closure -------------
{
    my $counter = 0;
    sub make-writer(\p) { return sub { p = 5 } }
    my $w = make-writer($counter);
    $w();
    is $counter, 5, 'raw param captured by a returned closure writes to the caller';
}

# --- 6-8: multi-hop raw relay (the alias chain must be transitive) ----------
{
    my $counter = 0;
    sub hop-inner(\p) { my $x := p; $x = 42 }
    sub hop-mid(\p)   { hop-inner(p) }
    sub hop-outer(\p) { hop-mid(p) }
    hop-outer($counter);
    is $counter, 42, 'three-hop raw relay: a := alias at the leaf reaches the caller';
}
{
    my $counter = 0;
    sub relay-inner(\p) { p = 7 }
    sub relay-outer(\p) { relay-inner(p) }
    relay-outer($counter);
    is $counter, 7, 'two-hop raw relay: a direct write at the leaf reaches the caller';
}
{
    # The exact shape roast/S32-list/tail.t's PredictiveIterator uses: a raw
    # param relayed through a second raw param into an attribute bind, written
    # only on a later call. The outer name differs from the parameter name, so
    # this cannot pass by the name coincidence the old mechanism relied on.
    my $pull-count = 0;
    my class Chained {
        has $!seen;
        method !SET-SELF(\c) { $!seen := c; self }
        method new(\c) { self.bless!SET-SELF: c }
        method tick { $!seen++ }
    }
    my $obj = Chained.new($pull-count);
    $obj.tick;
    $obj.tick;
    is $pull-count, 2, 'raw param relayed through bless into an attribute bind, ticked later';
}

# --- 9-10: a raw param bound to a non-lvalue stays readonly -----------------
{
    sub raw-literal(\p) { p }
    is raw-literal(9), 9, 'raw param bound to a literal still reads its value';
    dies-ok { my &c = sub (\p) { p = 1 }; c(9) },
        'raw param bound to a literal is not writable';
}

# --- 11-12: `\p` still binds containers and objects by reference ------------
{
    my @src = 1, 2, 3;
    sub raw-array(\p) { p.push(4) }
    raw-array(@src);
    is @src, [1, 2, 3, 4], 'raw param aliases an array argument';
}
{
    my %src = a => 1;
    sub raw-hash(\p) { p<b> = 2 }
    raw-hash(%src);
    is %src, {a => 1, b => 2}, 'raw param aliases a hash argument';
}

# --- 13: a raw param does NOT alias an unrelated same-named caller lexical --
{
    my $p = 'OUTER';
    my $target = 0;
    sub shadowing(\p) { my $x := p; $x = 3 }
    shadowing($target);
    is $target, 3, 'raw param writes to the argument it was given';
    is $p, 'OUTER', 'raw param leaves a same-named caller lexical alone';
}

# --- 15-16: capture / slurpy sigilless params are NOT implicitly raw --------
{
    my $counter = 0;
    sub cap-param(|c) { c[0] }
    is cap-param($counter), 0, 'a |c capture parameter still binds by value';
    my @vals = 1, 2;
    sub slurpy-sigilless(+a) { a.elems }
    is slurpy-sigilless(@vals), 2, 'a +a slurpy parameter still collects its arguments';
}

# --- 17-18: a TYPE NAME argument must not be turned into a container ---------
# `Compiler::positional_arg_source_name` records a bare `Expr::BareWord` arg
# source verbatim, so a class name reaches the binder looking exactly like a
# sigilless variable. Installing the shared cell under that name shadowed the
# CLASS with a `ContainerRef` for the rest of the program, and every later
# coercion into it failed (roast S12-coercion/coercion-methods.t). The
# `WrapVarRef` slot sentinel distinguishes the two.
{
    my class Coercible {
        has $.value;
        method COERCE(Numeric:D $n) { self.new: :value($n * 2) }
    }
    sub takes-type(Any $v, Mu \target) { $v ~~ target }
    my Coercible(Any) $c;
    $c = 3;
    ok takes-type($c, Coercible), 'a type name passed to a raw param type-checks';
    my Coercible(Any) $c2;
    $c2 = 4;
    is $c2.value, 8,
        'a later coercion into the same class still works after passing it to a raw param';
}
