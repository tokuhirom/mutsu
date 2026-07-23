use v6;
use Test;

# CALLER:: / CALLERS:: used from inside an *immediate* block (a bare block /
# `if` / `for` / `while` body) resolves against the block's lexical parent,
# because such a block is run in place and its dynamic caller IS its lexical
# outer scope. See roast S02-names/pseudo-6c.t "inline blocks" and the
# `in_immediate_block` compiler routing (CALLER:: -> OUTER::, CALLERS:: ->
# OUTERS::). Both the literal spelling and the indirect `$::($name)::x` form
# must agree.

plan 15;

# --- $CALLER:: in an inline if-block ---
{
    my $caller = 'CALLER';
    my $y is dynamic = 93;
    if 1 {
        is $CALLER::y, 93, '$CALLER::y works in an inline if-block';
        is $::($caller)::y, 93, 'indirect ::("CALLER")::y works in an inline block';
        is CALLER::<$y>, 93, 'CALLER::<$y> stash form works in an inline block';
    }
}

# --- $CALLERS:: (cascading) in an inline block reaches a grandparent scope ---
{
    my $callers = 'CALLERS';
    my $z is dynamic = 77;
    if 1 {
        if 1 {
            is $CALLERS::z, 77, '$CALLERS::z cascades past the immediate parent';
            is $::($callers)::z, 77, 'indirect ::("CALLERS")::z cascades';
            is CALLERS::<$z>, 77, 'CALLERS::<$z> stash form cascades';
        }
    }
}

# --- the innermost immediate parent wins for CALLER:: (exactly one scope) ---
{
    my $a is dynamic = 1;
    if 1 {
        my $a is dynamic = 2;
        if 1 {
            is $CALLER::a, 2, '$CALLER:: names the immediate lexical parent, not the outermost';
        }
    }
}

# --- CALLER:: does NOT cascade to a grandparent (unlike CALLERS::) ---
{
    my $g is dynamic = 5;
    if 1 {
        if 1 {
            ok !defined($CALLER::g), '$CALLER:: does not cascade past the immediate parent';
        }
    }
}

# --- for/while bodies are immediate blocks too ---
{
    my $w is dynamic = 42;
    for 1 {
        is $CALLER::w, 42, '$CALLER:: works from a for-loop body';
    }
    my $u is dynamic = 43;
    my $once = 0;
    while $once == 0 {
        $once = 1;
        is $CALLER::u, 43, '$CALLER:: works from a while-loop body';
    }
}

# --- a closure passed to a sub keeps *dynamic*-caller CALLER:: semantics ---
# (regression guard: the immediate-block routing must NOT touch this path)
{
    sub run($f) { my $x is dynamic = 90; $f() }
    is run({ $CALLER::x }), 90, 'CALLER:: in a passed closure sees the dynamic caller';
    is run({ CALLER::<$x> }), 90, 'CALLER::<$x> in a passed closure sees the dynamic caller';
}

# --- an inline block *inside* a closure: CALLER:: is the closure body ---
{
    sub run2($f) { my $c is dynamic = 7; $f() }
    my $r = run2(-> {
        my $inner is dynamic = 11;
        if 1 {
            # caller of the if-block is the closure body (has $inner)
            $CALLER::inner;
        }
    });
    is $r, 11, 'CALLER:: from an inline block inside a closure names the closure body';
}

# --- non-dynamic binding through CALLER:: from an inline block throws ---
# (the enclosing block's `$x` is a plain lexical; reaching it via CALLER:: must
# raise X::Caller::NotDynamic, exactly as the routine-frame CALLER:: path does)
dies-ok { my $x = 5; { CALLER::<$x> } },
    'CALLER:: on a non-dynamic binding from an inline block throws X::Caller::NotDynamic';

# --- a plain lexical (not dynamic) reached via CALLERS:: from an inline block ---
# (CALLERS:: is lenient about dynamic-ness, matching OUTERS::)
{
    my $p = 88;
    if 1 {
        is $CALLERS::p, 88, 'CALLERS:: reaches a plain lexical parent binding';
    }
}
