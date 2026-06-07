use v6;
use Test;

# Regression pin for Slice 6.3 step 2: the smartmatch (`~~`) opcode no longer
# sets a blanket `env_dirty = true`. Instead it reverse-write-throughs the lhs
# alias (`$x ~~ ...`) into its local slot. A caller's own locals must stay
# coherent across a smartmatch, and a smartmatch that mutates the lhs via the
# topic (s///, tr///) must propagate to the caller. See docs/vm-dual-store.md.

plan 14;

# --- caller local survives a literal-lhs smartmatch in a loop ---
{
    my $count = 0;
    my $iters = 0;
    for ^50 {
        if "x 42 y" ~~ /\d+/ { $count++ }
        $iters++;
    }
    is $count, 50, 'match count correct across loop';
    is $iters, 50, 'caller local untouched by smartmatch';
}

# --- capture vars after a match are readable ---
{
    my $x = "abc123";
    if $x ~~ /(\d+)/ {
        is ~$0, '123', 'capture $0 readable after match';
        is ~$/, '123', 'match $/ readable after match';
    }
    is $x, 'abc123', 'lhs var unchanged by a read-only match';
}

# --- substitution via topic alias mutates a local lhs ---
{
    my $s = "hello world";
    $s ~~ s/world/raku/;
    is $s, 'hello raku', 's/// through ~~ mutates local lhs (reverse write-through)';
}

# --- substitution in a sub, returned value reflects the mutation ---
sub subst-in-sub {
    my $s = "one two three";
    $s ~~ s/two/2/;
    return $s;
}
is subst-in-sub(), 'one 2 three', 's/// through ~~ visible to caller return';

# --- transliteration via topic alias mutates a local lhs ---
{
    my $t = "abc";
    $t ~~ tr/a..c/A..C/;
    is $t, 'ABC', 'tr/// through ~~ mutates local lhs';
}

# --- loop counter + lhs both locals: both stay coherent ---
{
    my $count = 0;
    my $str = "x1 x2 x3";
    while $str ~~ s/x\d/Y/ {
        $count++;
    }
    is $count, 3, 'loop counter local coherent across mutating smartmatch';
    is $str, 'Y Y Y', 'lhs local fully substituted';
}

# --- interleaved unrelated local reads around smartmatch ---
{
    my $acc = 0;
    my @vals = "a1", "b", "c3", "d";
    for @vals -> $v {
        $acc += 100;
        if $v ~~ /\d/ { $acc += 1 }
    }
    is $acc, 402, 'unrelated local accumulates correctly around smartmatch';
}

# --- given/when (smartmatch under the hood) keeps outer local coherent ---
{
    my $hits = 0;
    for 1..5 -> $n {
        given $n {
            when * %% 2 { $hits += 10 }
            default     { $hits += 1 }
        }
    }
    is $hits, 23, 'given/when smartmatch keeps outer accumulator coherent';
}

# --- nested sub call inside smartmatch RHS still propagates its own writes ---
{
    my $side = 0;
    sub bump-side { $side++; return /\d+/ }
    my $r = "42" ~~ bump-side();
    ok $r, 'smartmatch against a sub-returned regex matches';
    is $side, 1, 'side effect of RHS sub call propagates to caller';
}
