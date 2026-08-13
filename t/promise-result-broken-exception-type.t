use Test;

# `.result` on a Broken promise must re-throw the original cause with its
# real type intact -- a plain Str reason still gets wrapped in X::AdHoc (so
# `QUIT { default { .message } }`-style handlers keep working either way),
# but an object instance (a user exception the promise was broken with)
# must not be re-wrapped. A name-based "is this an exception" check cannot
# see a user class's `is Exception` ancestry when the class name is not
# literally `X::...` or `*Exception*` (found via ADR-0028 Slice 2's
# Cro-verification pass on `Cro::MessageWithBody.body-text`, whose
# `self.body-blob.then: -> $p { $p.result; ... }` shape depends on this).

plan 4;

class TooShort is Exception {
    method message() { "too short" }
}

# 1+2. .result on a promise broken with a user exception instance re-throws
# that exact type -- not X::AdHoc.
{
    my $p = Promise.new;
    $p.break(TooShort.new);
    my $caught;
    try { $p.result; CATCH { default { $caught = $_; } } }
    isa-ok $caught, TooShort, ".result on a promise broken with a user exception re-throws its real type";
    is $caught.message, "too short", "...and the message is preserved";
}

# 3. .result on a promise broken with a plain Str still wraps in X::AdHoc
# (the pre-existing, still-correct behavior for that shape).
{
    my $p = Promise.new;
    $p.break("plain string reason");
    my $caught;
    try { $p.result; CATCH { default { $caught = $_; } } }
    isa-ok $caught, X::AdHoc, ".result on a promise broken with a plain Str still wraps it in X::AdHoc";
}

# 4. The full Cro::MessageWithBody.body-text shape: .then's callback reading
# a broken antecedent's .result re-throws with the type intact, through
# `try await`.
{
    my $p = Promise.new;
    my $derived = $p.then: -> $antecedent { $antecedent.result };
    $p.break(TooShort.new);
    my $caught;
    try { await $derived; CATCH { default { $caught = $_; } } }
    isa-ok $caught, TooShort, ".then's callback reading a broken antecedent's .result preserves the exception type through await";
}

# vim: expandtab shiftwidth=4
