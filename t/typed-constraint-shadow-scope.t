use v6;
use Test;

# A typed `my TYPE $x` / `my TYPE @a` declared inside a branch or loop body
# that SHADOWS an already-existing outer binding of the same name must not
# leave its constraint behind on the outer variable when the body exits.
#
# This is the "outer-first shadow" shape, as opposed to the "fresh-after"
# shape pinned by t/typed-constraint-scope-matrix.t (where the outer variable
# is declared AFTER the inner scope exits). It used to leak for every
# branch/loop construct and both sigils, and was tracked as
# todo/deep/scoped-type-declaration-tags-the-shadowed-outer-value.md; this
# file replaces that ticket's expected-failing pin
# (t/typed-constraint-shadow-leak-unfixed.t).
#
# Root cause: `exec_set_var_type` overwrites the env-scoped
# `__mutsu_type::<name>` metadata for the declared name, and nothing put the
# outer binding's own metadata back. `exec_block_local_scope_op`'s exit
# cleanup deliberately skips every name that already existed in `env` before
# the branch (those are `pop_loop_local_scope`'s job), and loop bodies had no
# exit cleanup for the metadata at all. Fixed by recording the
# pre-declaration metadata into the innermost branch/loop scope at the moment
# the type-constraint op overwrites it, so `pop_loop_local_scope` restores it.
#
# Every row below is verified against real raku, which accepts all of them.

# ---------------------------------------------------------------------------
# Scalars, inside a routine.
# ---------------------------------------------------------------------------
lives-ok { sub f { my $x; if True { my Str $x = "a"; }; $x = 42 }; f() },
    'routine, if-branch shadow: outer $x usable after the branch';
lives-ok { sub f { my $x; unless False { my Str $x = "a"; }; $x = 42 }; f() },
    'routine, unless-branch shadow: outer $x usable after the branch';
lives-ok { sub f { my $x; if False { } else { my Str $x = "a"; }; $x = 42 }; f() },
    'routine, else-branch shadow: outer $x usable after the branch';
lives-ok { sub f { my $x; my $i = 0; while $i < 1 { my Str $x = "a"; $i++ }; $x = 42 }; f() },
    'routine, while-body shadow: outer $x usable after the loop';
lives-ok { sub f { my $x; loop (my $i = 0; $i < 1; $i++) { my Str $x = "a"; }; $x = 42 }; f() },
    'routine, C-style-loop-body shadow: outer $x usable after the loop';
lives-ok { sub f { my $x; my $i = 0; repeat { my Str $x = "a"; $i++ } while $i < 1; $x = 42 }; f() },
    'routine, repeat-body shadow: outer $x usable after the loop';
lives-ok { sub f { my $x; for 1..1 { my Str $x = "a"; }; $x = 42 }; f() },
    'routine, for-body shadow: outer $x usable after the loop';

# The outer scalar keeps its own value across the shadowing declaration.
{
    sub keeps-value { my $x = 1; if True { my Str $x = "a"; }; $x }
    is keeps-value(), 1, 'shadowed outer $x keeps its own value';
}

# ---------------------------------------------------------------------------
# Containers, inside a routine.
# ---------------------------------------------------------------------------
lives-ok { sub f { my @a; if True { my Int @a; @a.push(5) }; @a.push("x") }; f() },
    'routine, if-branch container shadow: outer @a usable after the branch';
lives-ok { sub f { my @a; unless False { my Int @a; @a.push(5) }; @a.push("x") }; f() },
    'routine, unless-branch container shadow: outer @a usable after the branch';
lives-ok { sub f { my @a; if False { } else { my Int @a; @a.push(5) }; @a.push("x") }; f() },
    'routine, else-branch container shadow: outer @a usable after the branch';
lives-ok { sub f { my @a; my $i = 0; while $i < 1 { my Int @a; @a.push(5); $i++ }; @a.push("x") }; f() },
    'routine, while-body container shadow: outer @a usable after the loop';
lives-ok { sub f { my @a; loop (my $i = 0; $i < 1; $i++) { my Int @a; @a.push(5) }; @a.push("x") }; f() },
    'routine, C-style-loop-body container shadow: outer @a usable after the loop';
lives-ok { sub f { my @a; my $i = 0; repeat { my Int @a; @a.push(5); $i++ } while $i < 1; @a.push("x") }; f() },
    'routine, repeat-body container shadow: outer @a usable after the loop';
lives-ok { sub f { my @a; for 1..1 { my Int @a; @a.push(5) }; @a.push("x") }; f() },
    'routine, for-body container shadow: outer @a usable after the loop';

lives-ok { sub f { my %h; if True { my Int %h; %h<k> = 5 }; %h<k> = "x" }; f() },
    'routine, if-branch hash shadow: outer %h usable after the branch';
lives-ok { sub f { my %h; my $i = 0; while $i < 1 { my Int %h; %h<k> = 5; $i++ }; %h<k> = "x" }; f() },
    'routine, while-body hash shadow: outer %h usable after the loop';
lives-ok { sub f { my %h; if True { my %h{Int}; %h{1} = 5 }; %h<k> = "x" }; f() },
    'routine, if-branch object-hash shadow: outer %h usable after the branch';

# The outer container keeps its own contents across the shadowing declaration.
{
    sub keeps-elems { my @a = 1, 2; if True { my Int @a; @a.push(5) }; @a.elems }
    is keeps-elems(), 2, 'shadowed outer @a keeps its own elements';
}

# ---------------------------------------------------------------------------
# The same shapes at mainline (outside any routine), where the declaration
# used to also write the process-global `var_type_constraints` map.
# ---------------------------------------------------------------------------
{
    my $mx;
    if True { my Str $mx = "a"; }
    lives-ok { $mx = 42 }, 'mainline, if-branch shadow: outer $mx usable after the branch';
}
{
    my $mw;
    my $i = 0;
    while $i < 1 { my Str $mw = "a"; $i++ }
    lives-ok { $mw = 42 }, 'mainline, while-body shadow: outer $mw usable after the loop';
}
{
    my $mf;
    for 1..1 { my Str $mf = "a"; }
    lives-ok { $mf = 42 }, 'mainline, for-body shadow: outer $mf usable after the loop';
}
{
    my $mr;
    my $i = 0;
    repeat { my Str $mr = "a"; $i++ } while $i < 1;
    lives-ok { $mr = 42 }, 'mainline, repeat-body shadow: outer $mr usable after the loop';
}
{
    my $ml;
    loop (my $i = 0; $i < 1; $i++) { my Str $ml = "a"; }
    lives-ok { $ml = 42 }, 'mainline, C-style-loop-body shadow: outer $ml usable after the loop';
}
{
    my @ma;
    if True { my Int @ma; @ma.push(5); }
    lives-ok { @ma.push("x") }, 'mainline, if-branch container shadow: outer @ma usable after the branch';
}
{
    my @mb;
    for 1..1 { my Int @mb; @mb.push(5); }
    lives-ok { @mb.push("x") }, 'mainline, for-body container shadow: outer @mb usable after the loop';
}

# ---------------------------------------------------------------------------
# The fix must not disable enforcement where it belongs. The inner typed
# declaration still enforces INSIDE the body, and a typed OUTER declaration
# still enforces after an inner shadow has come and gone.
# ---------------------------------------------------------------------------
dies-ok { sub f { my $x; if True { my Str $x; $x = 42 } }; f() },
    'inner typed scalar still enforces inside the branch';
dies-ok { sub f { my @a; if True { my Int @a; @a.push("s") } }; f() },
    'inner typed container still enforces inside the branch';
dies-ok { sub f { my $x; my $i = 0; while $i < 1 { my Str $x; $i++; $x = 42 } }; f() },
    'inner typed scalar still enforces inside the loop body';

dies-ok { sub f { my Str @a; if True { my Int @a; }; @a.push(1) }; f() },
    'typed outer container still enforces after an inner shadow';
{
    my Str @ta;
    for 1..1 { my Int @ta; }
    dies-ok { @ta.push(1) }, 'mainline typed outer container still enforces after an inner shadow';
}

# A scalar's constraint now rides on its ContainerRef cell just like an array
# or hash constraint rides on the container value.  Restoring a shadowed outer
# scalar therefore restores its enforcement without name-keyed metadata.
{
    dies-ok { sub f { my Str $x; if True { my Int $x = 1; }; $x = 42 }; f() },
        'typed outer scalar still enforces after an inner shadow';
}
{
    dies-ok { sub f { my Str $x; for 1..1 { my Int $x = 1; }; $x = 42 }; f() },
        'typed outer scalar still enforces after an inner for-body shadow';
}
{
    my Str $tx;
    if True { my Int $tx = 1; }
    dies-ok { $tx = 42 }, 'mainline typed outer scalar still enforces after an inner shadow';
}

# A typed declaration in a loop body enforces on EVERY iteration, not just the
# first (the metadata save is first-write-wins per loop, the registration is
# per-iteration).
dies-ok {
    sub f {
        my $x;
        my $i = 0;
        while $i < 3 { my Str $x; $i++; $x = 42 if $i == 3 }
    };
    f()
}, 'inner typed scalar still enforces on a later loop iteration';

done-testing;
