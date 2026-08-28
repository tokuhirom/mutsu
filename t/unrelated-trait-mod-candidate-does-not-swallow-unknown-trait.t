use Test;

plan 4;

# `has_trait_mod` only proves SOME `trait_mod:<is>` multi candidate exists
# somewhere -- it says nothing about whether any of them actually claims a
# GIVEN trait. When dispatch genuinely finds no matching candidate for the
# trait in question (`is_trait_mod_no_candidate`), that verdict was being
# swallowed unconditionally and the sub declaration silently kept the
# trait undecorated instead of reporting "unknown trait" -- exactly the
# same class of bug the sibling `vm_var_trait_ops.rs` fix (see
# `news/2026-08/user-trait-mod-does-not-consume-every-trait.md`) already
# fixed for VARIABLE traits, but left open for ROUTINE traits.
multi trait_mod:<is>(Routine:D $r, :$test-assertion!) {
    # Never actually called by this test -- its mere existence is what
    # used to make an unrelated trait name silently succeed.
}

{
    my $threw = False;
    my $message = '';
    try {
        EVAL 'sub yulia is krassivaya { }';
        CATCH {
            default {
                $threw = True;
                $message = .message;
            }
        }
    }
    ok $threw, 'an unrelated trait_mod:<is> candidate does not swallow an unknown trait';
    ok $message ~~ /'unknown trait'/,
        'the raised message still names it an unknown trait';
}

# A trait the SAME candidate genuinely does claim keeps working.
{
    my $ok = True;
    try {
        EVAL 'sub greet() is test-assertion { }';
        CATCH { default { $ok = False; } }
    }
    ok $ok, 'a trait a real candidate claims is still accepted';
}

# A second, differently-shaped unrelated trait name is rejected the same
# way (not just the exact one this file's own candidate declares against).
{
    my $threw = False;
    try {
        EVAL 'sub f() is this-is-not-a-real-trait-at-all { }';
        CATCH { default { $threw = True; } }
    }
    ok $threw, 'a second unrelated trait name is rejected the same way';
}
