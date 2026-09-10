use Test;

# Regression pin: `method dispatch:<...>` (a custom-dispatch override, e.g.
# `dispatch:<.?>` overriding the `.?method` fallback) is a category name
# valid ONLY on a `method`/`submethod` declaration, never a plain `sub`.
# `dispatch` was previously missing from the operator-category allowlist
# entirely, so even the method form raised "Cannot add tokens of category
# 'dispatch'" -- blocking `Font::AFM.rakumod` (a Text::CSV/CSV::Table
# transitive dependency) from parsing at all.
#
# Verified against a live rakudo (2026.06): `sub dispatch:<.?> {}` raises
# the exact same category error mutsu already raised for every category;
# `method`/`submethod dispatch:<.?>` compiles (and, per rakudo, only affects
# `.?` dispatch -- an ordinary `.missing-method` call still dies "No such
# method", since `dispatch:<.?>` is not a FALLBACK replacement). This test
# only pins the PARSE fix; it does not assert full custom-dispatch
# invocation semantics, which rakudo itself does not document and which
# this ticket did not need to implement to unblock Font::AFM's own
# TWEAK-time construction path.
#
# See todo/tickets/method-dispatch-colon-question-syntax.md /
# news/2026-08/method-dispatch-colon-question-syntax.md.

plan 4;

# (1) `method dispatch:<.?>` parses and the class compiles.
{
    my $ok = True;
    class Foo1 {
        method dispatch:<.?>(\name, |c) is raw {
            self.can(name) ?? self."{name}"(|c) !! Nil
        }
        method known() { 'known!' }
    }
    ok $ok, 'class with method dispatch:<.?> parses';
    is Foo1.new.known, 'known!', 'an ordinary declared method still dispatches normally';
}

# (2) `submethod dispatch:<.?>` parses too (rakudo accepts both).
{
    my $ok = True;
    class Foo2 {
        submethod dispatch:<.?>(\name, |c) is raw { Nil }
    }
    ok $ok, 'class with submethod dispatch:<.?> parses';
}

# (3) `sub dispatch:<.?>` (outside a class) is still correctly rejected --
#     `dispatch` remains an invalid category for a plain sub, matching
#     rakudo's own "Cannot add tokens of category 'dispatch'" error.
{
    my $threw = False;
    try {
        EVAL 'sub dispatch:<.?>(\name, |c) { 42 }';
        CATCH { default { $threw = True } }
    }
    ok $threw, 'sub dispatch:<.?> (non-method) is rejected';
}
