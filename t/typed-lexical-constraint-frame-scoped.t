use v6;
use Test;

# A typed scalar `my` inside a routine must not leak its constraint onto a
# same-named variable in another frame (the bare-name-keyed constraint store
# was scope-blind — todo/deep/bare-name-type-constraint-store-is-scope-blind.md,
# Text::CSV t/66_formula.t line 129).

plan 12;

# The Text::CSV shape: a module method's `my Str $e = ...` must not poison
# the caller script's untyped `$e` declared BEFORE the call.
{
    class TC { method m () { my Str $s = "x"; $s } }
    my $s;
    TC.m;
    lives-ok { $s = 42 }, 'method-scoped my Str $s does not constrain caller $s';
    is $s, 42, 'caller $s holds the assigned Int';
}

# Same shape through a plain sub call (0-arg fast path).
{
    sub leak0 { my Str $e = "x"; $e }
    my $e;
    leak0; leak0; leak0;
    lives-ok { $e = 42 }, 'sub-scoped my Str $e does not constrain caller $e';
}

# The overwrite shape: the callee's constraint must not REPLACE the caller's
# own constraint on a same-named typed lexical.
{
    sub over(Int $x is copy) { $x = 5 }
    my Str $x = "a";
    over(3);
    lives-ok { $x = "b" }, 'caller Str constraint survives callee Int param';
    throws-like { $x = 42 }, Exception, message => /'expected Str'/,
        'caller Str constraint still enforced after the call';
}

# Enforcement INSIDE the routine still works (env-scoped registration).
{
    sub inside { my Str $s = "x"; $s = 42; }
    dies-ok { inside() }, 'constraint enforced inside the declaring routine';
}

# A closure escaping the frame keeps enforcement through its captured env.
{
    sub outer { my Str $c = "a"; sub { $c = 42 } }
    my &esc = outer();
    dies-ok { esc() }, 'escaped closure still enforces the dead frame constraint';
}

# EVAL'd re-assignment inside the declaring frame sees the constraint.
{
    subset EvenT of Int where * %% 2;
    sub evens { my EvenT $n = 2; EVAL '$n = 3'; }
    dies-ok { evens() }, 'subset constraint enforced via EVAL in the frame';
}

# An unassigned typed scalar in a routine still reads as its type object.
{
    sub unassigned { my Str $u; $u.^name }
    is unassigned(), 'Str', 'Nil-valued typed routine lexical reads as type object';
}

# state variables keep enforcement too.
{
    sub st { state Str $s = "s"; $s = 99; }
    dies-ok { st() }, 'state Str constraint enforced in routine';
}

# A typed my inside a nested block of a method is frame-scoped as well.
{
    class TB { method m { if True { my Int $q = 1; } } }
    my $q;
    TB.m;
    lives-ok { $q = "str" }, 'block-in-method typed my does not leak';
}

# An outer typed lexical keeps enforcement while a routine shadows the name.
{
    sub shadow { my Int $o = 1; $o }
    my Str $o = "s";
    shadow();
    throws-like { $o = 42 }, Exception, message => /'expected Str'/,
        'outer Str constraint intact after routine-scoped Int shadow';
}
