use v6;
use Test;

# Regression: `$!` is lexical in a block — a block called from inside another
# routine must still see the `$!` from its own creation scope, the same way
# `$_` already does (`vm_closure_dispatch.rs`'s closure-entry merge). The
# merge's default "don't overwrite an existing name" (`entry_or_insert_sym`)
# used the chain-walking `contains_key_sym`, so a block bound to a `&`-sigiled
# parameter and called from inside a `sub` saw the *caller sub's own* fresh
# `$!` reset (visible through the parent chain) instead of its own captured
# value — losing the block's real `$!` entirely. Concretely, this made
# `dies-ok { $!.message }` (exactly `Test.rakumod`'s own `dies-ok` shape)
# silently succeed instead of dying, since `Nil.message` does not raise the
# way `Any.message` does. Found investigating `t/exception-methods.t`'s
# regression under `MUTSU_REAL_TEST=1`.

plan 3;

sub calls-code(&code) {
    my $died = False;
    try {
        code();
        CATCH { default { $died = True } }
    }
    $died;
}

try { 1 + 1 };
ok calls-code({ $!.message }),
    'a block bound to a &-sigiled param sees its own captured $! (not the caller sub\'s fresh reset)';

my $topic-seen;
for 'abc' {
    calls-code({ $topic-seen = $_ });
}
is $topic-seen, 'abc',
    'the same mechanism keeps working for $_ (not just $!)';

try { die "boom" };
my $msg;
calls-code({ $msg = $!.message });
is $msg, 'boom',
    'a genuinely-set $! is captured correctly too, not just Any';
