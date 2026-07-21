use v6;
use Test;

# A `my enum` (lexically scoped) is private to the role body and, unlike a
# default our-scoped enum, is allowed inside a role. An `our`/bare enum inside a
# role must still be rejected. Also covers: a type declared in the role body
# used as a parameter constraint, and a role type parameter (`[::KeyT]`)
# supplied AFTER the `:ver`/`:auth` adverbs.

plan 6;

# 1. `my enum` inside a role is accepted.
{
    role R1 {
        my enum TOrder is export <DESC ASC>;
        method first-order { TOrder::DESC }
    }
    class C1 does R1 { }
    is C1.new.first-order.key, 'DESC', 'my enum inside a role works';
}

# 2. A bare (default our-scoped) enum inside a role is rejected.
{
    my $err = '';
    try {
        EVAL 'role RBad { enum EBad <A B>; }';
        CATCH { default { $err = .message } }
    }
    like $err, /'our-scoped enum'/, 'bare enum inside a role is rejected';
}

# 3. An `our`-scoped enum inside a role is rejected.
{
    my $err = '';
    try {
        EVAL 'role ROur { our enum EOur <A B>; }';
        CATCH { default { $err = .message } }
    }
    like $err, /'our-scoped enum'/, 'our enum inside a role is rejected';
}

# 4. A body-declared enum type used as a parameter constraint is accepted (the
#    role used to fail to register with "Invalid typename 'Color'"). The enum is
#    private to the role, so feed the value from inside a sibling method.
{
    role R4 {
        my enum Color is export <Red Green>;
        method paint(Color $c) { $c.key }
        method paint-green { self.paint(Green) }
    }
    class C4 does R4 { }
    is C4.new.paint-green, 'Green', 'body-declared enum usable as param type';
}

# 5. A role type parameter declared with `[::KeyT]` (no adverbs) constrains
#    method params.
{
    role R5[::KeyT] {
        method wrap(KeyT $k) { $k }
    }
    class C5 does R5[Int] { }
    is C5.new.wrap(42), 42, 'plain role type param constrains method param';
}

# 6. A role type parameter supplied AFTER :ver/:auth adverbs is still captured,
#    so the param constraint resolves (this used to error "Invalid typename").
{
    my $ok = 0;
    try {
        EVAL 'role R6:ver<1.0>:auth<zef:me>[::KeyT] { method wrap(KeyT $k) { $k } }; my $r = 1;';
        $ok = 1;
    }
    ok $ok, 'role type param after :ver/:auth adverbs is captured';
}
