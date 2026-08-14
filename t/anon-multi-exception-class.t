use Test;

plan 10;

# `only`/`multi`/`proto sub {}` (and `multi sub (...) {}`) with no name is a
# fatal compile-time error in Raku, X::Anon::Multi. The parser raised it via
# PError::fatal(), which itself prepends the "FATAL:" sentinel prefix used to
# mark a parse error as non-recoverable — but every call site *also* wrote a
# literal "FATAL:" in the message text, so the stored message doubled up to
# "FATAL:FATAL:X::Anon::Multi: ...". PError's Display strips only one
# "FATAL:" layer, leaving a residual "FATAL:X::Anon::Multi: ..." that no
# longer starts with "X::" — so the "X::Type: text" convention
# (RuntimeError::split_typed_message_convention) failed to recognize the
# class and the error fell back to the generic X::Syntax::Confused instead of
# X::Anon::Multi. Regression test for that double-prefix bug.

throws-like { EVAL 'only sub {}' }, X::Anon::Multi,
    'anonymous only sub is an error';
throws-like { EVAL 'multi sub {}' }, X::Anon::Multi,
    'anonymous multi sub is an error';
throws-like { EVAL 'proto sub {}' }, X::Anon::Multi,
    'anonymous proto sub is an error';
throws-like { EVAL 'multi sub (Int $x) { }' }, X::Anon::Multi,
    'anonymous multi sub with a signature is an error';

# The message text itself must not still carry the "FATAL:" sentinel.
{
    my $e = (try { EVAL 'multi sub {}' }, $!).tail;
    isa-ok $e, X::Anon::Multi, 'caught in $! with its class';
    ok !$e.message.starts-with('FATAL:'), 'no leftover FATAL: sentinel in the message';
    ok $e.message.contains('anonymous routine'), 'and the real text survives';
}

# A CATCH block dispatches on the type, not just the message text.
{
    my $seen;
    {
        EVAL 'multi sub {}';
        CATCH {
            when X::Anon::Multi { $seen = 'anon-multi' }
            default { $seen = 'default' }
        }
    }
    is $seen, 'anon-multi', 'a typed when matches the compile error';
}

# `multi`/`only` as an ordinary call (not the declarator form) still works —
# the fatal check must not misfire on a ")" following the bare keyword.
{
    sub multi(Int $x) { $x + 1 }
    is multi(5), 6, 'multi(...) as an ordinary call still works';
}

{
    my $e = (try { EVAL 'only sub {}' }, $!).tail;
    ok $e.message.contains('only declarator'), 'only names its own declarator';
}
