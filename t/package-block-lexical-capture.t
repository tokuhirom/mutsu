use Test;

# A named sub defined inside a `package`/`module` BLOCK closes over the block's
# `my` lexicals. mutsu's registry subs have no per-sub closure env and resolve
# free vars from the call-time env, and the block scope is dropped on exit, so a
# by-name call (`Foo::f`) used to see the lexical as undefined. The package's
# `my` lexicals are now recorded per-package and a variable miss inside that
# package's subs falls back to them — without leaking to bare references after
# the block (which run under GLOBAL).
#
# Surfaced by zef: `package Zef::CLI { my $CONFIG = ...; sub MAIN { ... $CONFIG } }`.

plan 5;

# The lexical does NOT leak to a bare reference after the block (raku scopes it):
# evaluating such code dies. (Checked via is_run so the parse-time failure is
# observed as a non-zero exit rather than aborting this file.)
is_run 'package P3 { my $z = 1 }; say $z;', { status => { $_ != 0 }, err => /'not declared'/ },
    "package-block my-lexical does not leak after the block";

# Core case: by-name call to a package-block sub sees the block lexical.
{
    package P1 { my $X = "captured"; our sub show() { $X } }
    is P1::show(), "captured", "package-block sub sees the block my-lexical";
}

# module block too.
{
    module M1 { my $Y = 42; our sub get() { $Y } }
    is M1::get(), 42, "module-block sub sees the block my-lexical";
}

# The lexical does NOT leak to a bare reference after the block (raku scopes it).
{
    package P2 { my $secret = 99; our sub reveal() { $secret } }
    is P2::reveal(), 99, "in-package access works";
}

# A value computed at block mainline (not a literal) is visible to the sub.
{
    package P4 {
        my $derived = (1, 2, 3).map(* * 2).sum;
        our sub total() { $derived }
    }
    is P4::total(), 12, "computed block lexical visible to by-name sub";
}
