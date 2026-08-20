use v6;
use Test;

# ADR-0042 slice 1: a bare-name type-constraint map is the wrong architecture
# for containers -- enforcement now reads the constraint carried on the
# container VALUE itself (`ArrayData`/`HashData`'s `value_type`/`key_type`),
# not a separate name-keyed side table that is scope-blind. This file pins:
#
#   §2.2 the container matrix: a typed `@`/`%` declared in some inner scope
#   must not poison a same-named UNTYPED container used after that scope has
#   exited.
#
#   §2.1 the scalar matrix's `if`/`unless`/`else` rows: slice 1 step 4 taught
#   `BlockLocalScope`'s exit cleanup to also strip the env-scoped
#   `__mutsu_type::`/`__mutsu_hash_key_type::` metadata keys, mirroring what
#   the genuine-block path (`BlockScope`) already did.
#
# Expected values verified against `raku` (every row here passes there).


# --- §2.2: container matrix (all 7 shapes must be green) ---

{
    sub inner { my Int @a; @a.push(5); }
    sub outer { inner(); my @a; @a.push("x"); @a[*-1] }
    is outer(), "x", 'container: routine-scoped my Int @a does not poison a later routine-local @a';
}

{
    sub via-block {
        { my Int @a; @a.push(5); }
        my @a; @a.push("x"); @a[*-1]
    }
    is via-block(), "x", 'container: bare-block-scoped my Int @a does not poison a later @a';
}

{
    sub via-if {
        if True { my Int @a; @a.push(5); }
        my @a; @a.push("x"); @a[*-1]
    }
    is via-if(), "x", 'container: if-branch my Int @a does not poison a later @a';
}

{
    sub via-while {
        my $i = 0;
        while $i < 1 { my Int @a; @a.push(5); $i++; }
        my @a; @a.push("x"); @a[*-1]
    }
    is via-while(), "x", 'container: while-body my Int @a does not poison a later @a';
}

{
    sub via-for {
        for 1..1 { my Int @a; @a.push(5); }
        my @a; @a.push("x"); @a[*-1]
    }
    is via-for(), "x", 'container: for-body my Int @a does not poison a later @a';
}

{
    sub via-hash {
        { my Int %h; %h<a> = 5; }
        my %h; %h<a> = "x"; %h<a>
    }
    is via-hash(), "x", 'container: block-scoped my Int %h does not poison a later %h';
}

{
    sub via-objhash {
        { my %h{Int}; %h{1} = "a"; }
        my %h; %h<a> = "x"; %h<a>
    }
    is via-objhash(), "x", 'container: block-scoped my %h{Int} does not poison a later %h';
}

# --- §2.1: if/unless/else scalar rows (now fixed by BlockLocalScope cleanup) ---

{
    sub via-if-scalar {
        if True { my Str $x = "a"; }
        my $x; $x = 42; $x
    }
    is via-if-scalar(), 42, 'scalar: if-branch my Str $x does not poison a later $x';
}

{
    sub via-unless-scalar {
        unless False { my Str $x = "a"; }
        my $x; $x = 42; $x
    }
    is via-unless-scalar(), 42, 'scalar: unless-branch my Str $x does not poison a later $x';
}

{
    sub via-else-scalar {
        if False { } else { my Str $x = "a"; }
        my $x; $x = 42; $x
    }
    is via-else-scalar(), 42, 'scalar: else-branch my Str $x does not poison a later $x';
}

# --- §3: alias probe -- enforcement reads the CONTAINER, not the name ---
# For every typed-container declaration shape, a differently-named bound
# alias still enforces. This is what proves enforcement reads the value's
# own embedded metadata and not the (scope-blind) name map.

{
    my Int @a1;
    my @x1 := @a1;
    dies-ok { @x1.push("bad") }, 'alias: fresh Int @a enforced through a differently-named alias';
}

{
    my Int @a2 = 1, 2;
    my @x2 := @a2;
    dies-ok { @x2.push("bad") }, 'alias: initializer-typed @a enforced through alias';
}

{
    my Int @a3;
    @a3 = 1, 2;
    my @x3 := @a3;
    dies-ok { @x3.push("bad") }, 'alias: whole-assigned Int @a enforced through alias';
}

{
    my Int %h1;
    my %y1 := %h1;
    dies-ok { %y1<k> = "bad" }, 'alias: my Int %h enforced through a differently-named alias';
}

{
    my %h2{Int};
    my %y2 := %h2;
    %y2{1} = 5;
    dies-ok { %y2{"bad"} = 5 }, 'alias: my %h{Int} key type enforced through alias';
}

done-testing;
