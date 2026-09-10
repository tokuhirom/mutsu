use Test;

# Slice F (docs/vm-single-store.md): a `~~` regex match writes caller lexicals
# straight into `env` — the match variable `$/`, numbered captures, and embedded
# `{ }` code-block writes. The smartmatch site now writes those through to the
# caller's local slots immediately (`writeback_match_locals`), so the slots stay
# coherent WITHOUT the reverse `sync_locals_from_env` pull. These cases all pass
# in normal mode; running under `MUTSU_NO_REVERSE_SYNC=1` exercises the precise
# write-through path (the campaign diagnostic).

plan 7;

# --- embedded closure updates an outer lexical -------------------------------
{
    my $x = 3;
    my $y = 2;
    ok 'a' ~~ /. { $y = $x; }/, 'match with embedded closure';
    is $y, 3, 'embedded closure write reaches the caller slot';
}

# --- $/ is coherent after a match -------------------------------------------
{
    my $str = 'hello42world';
    ok $str ~~ /(\d+)/, 'match with a capture';
    is $/.Str, '42', '$/ slot is coherent after match';
    is $0.Str, '42', 'capture $0 slot is coherent after match';
}

# --- $/ updated across successive matches ------------------------------------
{
    my $s = 'abc';
    $s ~~ /(.)(.)/;
    is "$0$1", 'ab', 'first match captures';
    $s ~~ /(.)$/;
    is $0.Str, 'c', 'second match overwrites the capture slot';
}
