use Test;

# Slice C' (docs/vm-single-store.md, open-question #2): generalize the
# carrier-return blanket-`env_dirty` drop from EVAL to *every* fully-reconciled
# carrier (interpreter function fallbacks, regex subrule calls). The risk the
# drop must survive: a carrier writes a caller lexical through a path that
# bypasses `set_env_with_main_alias` (an embedded regex `{ }`/`:my`/`:let`
# block writes env directly). regex_eval.rs now logs those writes into the
# carrier set, so the writeback reconciles them precisely instead of leaning on
# the coarse barrier pull. These pins assert the reconcile stays correct without
# the blanket net.

plan 11;

# 1. regex with an embedded code block writing an outer lexical, run repeatedly
{
    my $seen = '';
    for <a1 b2 c3> -> $s {
        $s ~~ / $<d>=(\d) { $seen = ~$<d> } /;
        # each iteration's write must be visible immediately on the next read
        is $seen, $s.substr(1), "embedded-code write visible (\$s=$s)";
    }
}

# 2. regex :my declaration that the surrounding scope reads after the match
{
    my token withmy { :my $tag = 'X'; \d+ }
    ok '42' ~~ &withmy, ':my-declaring regex matches';
    is ~$/, '42', 'match captured the digits';
}

# 3. embedded write inside a regex called from a (non-EVAL) sub carrier
{
    my $tracked = -1;
    sub do-match($txt) {
        $txt ~~ / (\d+) { $tracked = +$0 } /;
        return ~$0;
    }
    my $r = do-match('val=99');
    is $r, '99', 'sub-carrier regex returned the capture';
    is $tracked, 99, 'outer lexical written by embedded code reconciled after sub call';
}

# 4. interleave a regex-carrier write with an unrelated hot local (benchmark shape)
{
    my $hits = 0;
    my $acc  = 0;
    my $step = 3;
    for 1..5 -> $i {
        "x{$i}" ~~ / (\d) { $hits = $hits + 1 } /;
        $acc = $acc + $step;
    }
    is $hits, 5, 'each regex-carrier iteration incremented the outer lexical';
    is $acc, 15, 'interleaved hot local untouched by the carrier drop';
}

# 5. successful vs failed match: a failed match must not leave a stale slot
{
    my $note = 'none';
    'abc' ~~ / (\d+) { $note = 'matched' } /;
    is $note, 'none', 'failed match did not run the embedded write';
    'a7c' ~~ / (\d+) { $note = 'matched' } /;
    is $note, 'matched', 'successful match ran the embedded write';
}
