# A `my @x = <block-valued expression>` whose initializer ends in a `}` block
# (gather {...}, do {...}, do for {...}) is self-terminating at end of line: the
# next line's `if`/`for`/etc. must start a NEW statement, NOT be swallowed as a
# postfix statement modifier. Regression for a parser bug where only `Stmt::Expr`
# (not `Stmt::VarDecl`/`Stmt::Assign`) block-final statements were guarded, so
# `my @a = gather {...}\nif COND {...}` mis-parsed `if COND` as a modifier
# (making the assignment conditional) and turned `{...}` into a separate bare
# block. Surfaced by zef's distribution-depends-parsing test (die propagation
# through a nested gather reification back to a `.first` block's CATCH).

use Test;
plan 6;

# --- my @x = gather {...} then a separate if ---
{
    my $branch = 0;
    my @alt = gather {
        take 1; take 2;
    }
    if False {
        $branch = 1;
    }
    is @alt.join(","), "1,2", "my @x = gather {} keeps its value (if not a modifier)";
    is $branch, 0, "the following if COND {} is a separate statement, not a modifier";
}

# --- my @x = gather for LIST -> $g {...} then a separate if ---
{
    my $branch = 0;
    my @alt = gather for (1, 2, 3) -> $g {
        take $g if $g %% 1;
    }
    if False {
        $branch = 1;
    }
    is @alt.join(","), "1,2,3", "my @x = gather for {} keeps its value";
    is $branch, 0, "if after gather-for block is a separate statement";
}

# --- my $x = do {...} then a separate if ---
{
    my $branch = 0;
    my $v = do {
        41 + 1;
    }
    if False {
        $branch = 1;
    }
    is $v, 42, "my \$x = do {} keeps its value";
    is $branch, 0, "if after do block is a separate statement";
}
