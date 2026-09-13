use Test;

# `++$p` / `--$p` handed straight to another `is rw` parameter.
#
# A NATIVE-typed `is rw` parameter is bound to a native reference to the
# caller's location, so rakudo's native prefix increment writes through that
# reference and yields the reference again -- which means the callee's own
# `is rw` parameter binds the ORIGINAL caller's storage, two frames up.
# JSON::Fast's whitespace scanner relies on exactly this: `nom-comment($text,
# ++$pos)` has to be able to un-eat the character it just consumed.
#
# The shape is narrow: rakudo accepts it ONLY when the operand is itself a
# native `is rw` parameter. Every other spelling is an error there, so the
# rejections below are as much a part of the pin as the acceptance.

plan 16;

# --- accepted: the increment and the callee's decrement cancel ---------------

{
    sub inner(int $p is rw) { --$p }
    sub outer(int $p is rw) { inner(++$p) }
    my int $pos = 5;
    outer($pos);
    is $pos, 5, '++$p to a native is-rw parameter binds the caller container';
}

{
    sub inner(int $p is rw) { ++$p }
    sub outer(int $p is rw) { inner(--$p) }
    my int $pos = 5;
    outer($pos);
    is $pos, 5, '--$p binds through the same way';
}

{
    sub c(int $p is rw) { --$p }
    sub b(int $p is rw) { c(++$p) }
    sub a(int $p is rw) { b(++$p) }
    my int $v = 0;
    a($v);
    is $v, 1, 'the native reference relays across three frames';
}

# The increment is still performed -- binding the container must not skip it.
{
    sub inner(int $p is rw) { }
    sub outer(int $p is rw) { inner(++$p) }
    my int $pos = 5;
    outer($pos);
    is $pos, 6, 'the increment itself still happens';
}

# A non-rw callee takes the incremented VALUE, and the caller still sees the
# increment (it happened through the reference before the call).
{
    my $seen;
    sub inner(int $x) { $seen = $x }
    sub outer(int $p is rw) { inner(++$p) }
    my int $pos = 5;
    outer($pos);
    is $seen, 6, 'a read-only callee receives the incremented value';
    is $pos, 6, 'and the caller sees the increment';
}

# The parameter stays lexically visible inside a nested block.
{
    sub inner(int $p is rw) { --$p }
    sub outer(int $p is rw) { if True { inner(++$p) } }
    my int $pos = 5;
    outer($pos);
    is $pos, 5, 'the shape works from inside a nested block';
}

# --- refused, exactly as rakudo refuses them --------------------------------

# A plain `my int` lexical is not a native reference: rakudo dies with
# "Expected a modifiable native int argument for '$p'".
sub plain-native-callee(int $p is rw) { --$p }
dies-ok {
    my int $m = 5;
    plain-native-callee(++$m);
}, '++ on a plain native lexical is still refused';

# ... while passing that same lexical WITHOUT the increment is fine, so the
# refusal above is about the argument shape and not about native binding.
{
    sub plain-pass-callee(int $p is rw) { --$p }
    my int $m = 5;
    plain-pass-callee($m);
    is $m, 4, 'the same lexical passed plainly still binds';
}

# A non-native `$p is rw` holds a Scalar, and `prefix:<++>` returns its value:
# rakudo dies with "expects a writable container ... as a value without a
# container".
sub nonnative-inner($p is rw) { $p-- }
sub nonnative-outer($p is rw) { nonnative-inner(++$p) }
dies-ok {
    my $s = 5;
    nonnative-outer($s);
}, '++ on a non-native rw parameter is still refused';

# Postfix `$p++` yields the OLD value, never a reference; rakudo refuses it.
sub postfix-inner(int $p is rw) { --$p }
sub postfix-outer(int $p is rw) { postfix-inner($p++) }
dies-ok {
    my int $q = 5;
    postfix-outer($q);
}, 'postfix $p++ is still refused';

# --- a conditional over the reference ----------------------------------------
#
# `nom-comment` hands the scan position on as `nom-ws($text, $ord == -1 ?? $pos
# !! ++$pos)`. Each arm of rakudo's conditional yields the native reference, so
# the conditional as a whole is still that reference -- whichever arm ran.

{
    sub inner(int $p is rw) { --$p }
    sub outer(int $p is rw) { inner(True ?? $p !! ++$p) }
    my int $pos = 5;
    outer($pos);
    is $pos, 4, 'the taken arm is a bare parameter: it binds, nothing increments';
}

{
    sub inner(int $p is rw) { --$p }
    sub outer(int $p is rw) { inner(False ?? $p !! ++$p) }
    my int $pos = 5;
    outer($pos);
    is $pos, 5, 'the taken arm is ++$p: it increments, then binds';
}

{
    sub inner(int $p is rw) { --$p }
    sub outer(int $p is rw) { inner(True ?? ++$p !! $p) }
    my int $pos = 5;
    outer($pos);
    is $pos, 5, 'the increment arm works on the then side too';
}

# Only the arm actually taken runs its increment.
{
    sub inner(int $p is rw) { }
    sub outer(int $p is rw) { inner(False ?? ++$p !! --$p) }
    my int $pos = 5;
    outer($pos);
    is $pos, 4, 'the untaken arm never increments';
}

# The two arms need not name the same parameter; the chosen reference binds.
{
    sub inner(int $p is rw) { --$p }
    sub outer(int $p is rw, int $q is rw) { inner(True ?? $p !! $q) }
    my int $a = 5;
    my int $b = 9;
    outer($a, $b);
    is "$a,$b", '4,9', 'a conditional over two parameters binds the chosen one';
}
