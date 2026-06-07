use v6;
use Test;

# Regression pin for Slice 6.3 step 2: the smartmatch / regex-match op
# (exec_smart_match_expr_op) sets env_dirty *precisely*. A side-effect-free
# match (a plain `/regex/` literal that runs no embedded `{ }` code block, with
# `$/` not a `my`-declared local) writes only `$/`/captures and needs no
# caller-locals re-sync. But a match that runs embedded code writing a caller
# variable (tracked via the engine's pending_local_updates), an adverbed match
# (`:pos`/`:g`), a named/Sub regex, or a substitution, must still propagate.
# See docs/vm-dual-store.md (Slice 6.3).

plan 18;

# --- pure regex match in a loop: caller local stays coherent ---
{
    my $count = 0;
    my $iters = 0;
    for ^50 {
        if "x 42 y" ~~ /\d+/ { $count++ }
        $iters++;
    }
    is $count, 50, 'pure match count correct across loop';
    is $iters, 50, 'caller local untouched by pure matches';
}

# --- capture vars readable after a pure match ---
{
    my $x = "abc123";
    if $x ~~ /(\d+)/ {
        is ~$0, '123', 'capture $0 readable after match';
        is ~$/, '123', 'match $/ readable after match';
    }
    is $x, 'abc123', 'lhs var unchanged by a read-only match';
}

# --- embedded code block writes a caller variable ---
{
    my $side = 0;
    "a" ~~ / a { $side = 99 } /;
    is $side, 99, 'embedded {} block write to caller var propagates';
}

# --- embedded block in a loop, interleaved with a caller counter ---
{
    my $hits = 0;
    my $n = 0;
    for ^10 {
        "a" ~~ / a { $hits = $hits + 1 } /;
        $n = $n + 1;
    }
    is $hits, 10, 'embedded-block writes accumulate across loop';
    is $n, 10, 'caller counter coherent alongside embedded-block matches';
}

# --- named regex with an embedded block writing a caller var ---
{
    my $v = 0;
    my regex foo { a { $v = 7 } }
    "a" ~~ &foo;
    is $v, 7, 'named regex embedded-block write propagates';
}

# --- substitution via topic alias mutates a local lhs ---
{
    my $s = "hello world";
    $s ~~ s/world/raku/;
    is $s, 'hello raku', 's/// through ~~ mutates local lhs';
}

# --- substitution in a sub, returned value reflects the change ---
sub subst-in-sub {
    my $s = "one two three";
    $s ~~ s/two/2/;
    return $s;
}
is subst-in-sub(), 'one 2 three', 's/// visible to caller return';

# --- my $/ : $/ is a local; continued match reads it across calls ---
{
    my $str = "abcabcabc";
    my $/;
    $str ~~ m:p/abc/;
    is $/.to, 3, 'first :pos match position';
    $str ~~ m:p/abc/;
    is $/.to, 6, 'continued :pos match reads prior $/ (local) correctly';
    $str ~~ m:p/abc/;
    is $/.to, 9, 'recontinued :pos match position';
}

# --- given/when (smartmatch) keeps outer accumulator coherent ---
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

# --- unrelated local read interleaved with pure matches ---
{
    my $acc = 0;
    my @vals = "a1", "b", "c3", "d";
    for @vals -> $v {
        $acc = $acc + 100;
        if $v ~~ /\d/ { $acc = $acc + 1 }
    }
    is $acc, 402, 'unrelated local accumulates correctly around pure matches';
}

# --- RHS sub-returned regex: side effect of the sub propagates ---
{
    my $side = 0;
    sub bump-side { $side = $side + 1; return /\d+/ }
    my $r = "42" ~~ bump-side();
    ok $r, 'smartmatch against a sub-returned regex matches';
    is $side, 1, 'side effect of RHS sub call propagates to caller';
}
