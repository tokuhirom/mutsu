use v6;
use Test;

# `OUTER::` names exactly ONE scope and `OUTERS::` names any outer scope
# (packages.rakudoc: "OUTER  Symbols in the next outer lexical scope" /
# "OUTERS  Symbols in any outer lexical scope"). Both spellings -- the symbolic
# `$OUTER::x` and the stash subscript `OUTER::<$x>` -- must agree.
# Every expectation below is the value rakudo prints for the same snippet.

plan 23;

# --- OUTER:: reaches exactly one scope out -------------------------------
my $y = 7;
{
    is $OUTER::y, 7, '$OUTER::y one scope out';
    is OUTER::<$y>, 7, 'OUTER::<$y> one scope out';
}
{
    {
        # Two scopes out is NOT OUTER's business: the enclosing block declares
        # no $y, so the answer is Nil -- not the file-scope $y.
        nok $OUTER::y.defined, '$OUTER::y does not cascade past one scope';
        nok OUTER::<$y>.defined, 'OUTER::<$y> does not cascade past one scope';
        is $OUTER::OUTER::y, 7, '$OUTER::OUTER::y reaches two scopes out';
        is OUTER::OUTER::<$y>, 7, 'OUTER::OUTER::<$y> reaches two scopes out';
    }
}

# --- a shadowing declaration is what the enclosing scope binds ------------
{
    my $y = 8;
    {
        is $OUTER::y, 8, '$OUTER::y sees the enclosing shadow';
        is OUTER::<$y>, 8, 'OUTER::<$y> sees the enclosing shadow';
    }
}

# --- @ and % keep their sigil in the stash key ---------------------------
my @a = 1, 2;
my %h = a => 1;
{
    is-deeply OUTER::<@a>, [1, 2], 'OUTER::<@a> resolves an array';
    is-deeply OUTER::<%h>, {a => 1}, 'OUTER::<%h> resolves a hash';
}

# --- routine scopes count as scopes --------------------------------------
sub one-out { $OUTER::y }
is one-out(), 7, '$OUTER::y from a routine body reaches the declaring scope';

sub nested-in-routine { { $OUTER::y } }
nok nested-in-routine().defined,
    '$OUTER::y in a routine-nested block stops at the routine body';

sub param-outer($p) { { $OUTER::p } }
is param-outer(42), 42, 'a parameter is a declaration of its routine scope';

my $closure = { $OUTER::y };
is $closure(), 7, '$OUTER::y from a stored closure reaches its declaring scope';

# A free variable merely *mentioned* in a body is not declared by it.
sub mentions-free { my $seen = $y; { $OUTER::y } }
nok mentions-free().defined,
    'a free variable used in a body is not a declaration of that body';

# --- OUTERS:: cascades outward -------------------------------------------
{
    {
        is $OUTERS::y, 7, '$OUTERS::y cascades to any outer scope';
        is OUTERS::<$y>, 7, 'OUTERS::<$y> cascades to any outer scope';
    }
}
{
    my $y = 8;
    # OUTERS excludes the *current* scope, so this is the file-scope 7.
    is $OUTERS::y, 7, 'OUTERS:: excludes the current scope';
    {
        is $OUTERS::y, 8, 'OUTERS:: stops at the innermost enclosing declaration';
    }
}
nok $OUTERS::OUTERS::y.defined, 'OUTERS:: does not chain';

# --- EVAL'd units sit behind a scope with no user lexicals ---------------
# rakudo runs an EVAL'd unit behind wrapper scopes holding no user lexicals,
# so OUTER:: from EVAL'd mainline code finds nothing -- even for a variable
# sitting in the block that called EVAL. Plain lookups still see the caller.
{
    ok EVAL(q{not OUTER::<$y>.defined}), 'OUTER:: from an EVAL unit is empty';
    is EVAL(q{$y}), 7, 'a plain lookup in an EVAL unit still sees the caller';
}
{
    # The my-6c.t:216 shape: the enclosing block declares $x *after* the EVAL.
    my $x = 0;
    {
        {
            ok EVAL(q{not OUTER::<$x>.defined}),
                'OUTER::<$x> from EVAL does not resolve to an enclosing $x';
            my $x; #OK not used
        }
    }
}
