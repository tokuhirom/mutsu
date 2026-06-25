use Test;

# Pre-existing §D(b) construction blocker: a nested method dispatch inside one
# BUILD submethod (`my $x = S.new`) consumed a *sibling* BUILD's captured-outer
# writeback via the drop-on-miss method-call op-tail drain, leaving the caller's
# slot stale (S12-construction/BUILD.t "Called Parent's BUILD method once").
# Fixed by routing a drop-on-miss source to the retain-on-miss caller-var list so
# the frame that owns the slot drains it.

plan 6;

# --- nested .new inside child BUILD must not drop parent BUILD's write ---
{
    my $pc = 0;
    class S1 { }
    class P1 { submethod BUILD { $pc++ } }
    class C1 is P1 { submethod BUILD { my $x = S1.new } }
    C1.new;
    is $pc, 1, 'nested .new in child BUILD keeps parent BUILD write';
}

# --- nested method call (with user method) inside child BUILD ---
{
    my $pc = 0;
    class S2 { method hi { 42 } }
    class P2 { submethod BUILD { $pc++ } }
    class C2 is P2 { submethod BUILD { my $x = S2.new.hi } }
    C2.new;
    is $pc, 1, 'nested user-method call in child BUILD keeps parent write';
}

# --- both BUILDs counted correctly with nested dispatch ---
{
    my $pc = 0;
    my $cc = 0;
    class S3 { }
    class P3 { submethod BUILD { $pc++ } }
    class C3 is P3 { submethod BUILD { $cc++; my $x = S3.new } }
    C3.new;
    is $pc, 1, 'parent counter == 1';
    is $cc, 1, 'child counter == 1';
}

# --- coercion (nested dispatch) inside child BUILD ---
{
    my $pc = 0;
    class N4 { method Numeric { 5 } }
    class P4 { submethod BUILD { $pc++ } }
    class C4 is P4 { submethod BUILD { my $x = +N4.new } }
    C4.new;
    is $pc, 1, 'nested coercion in child BUILD keeps parent write';
}

# --- ordinary method call writeback still works (no regression) ---
{
    my $calls = 0;
    class E5 { method bump { $calls++ } }
    my $e = E5.new;
    $e.bump;
    $e.bump;
    is $calls, 2, 'ordinary method-call writeback unchanged';
}
