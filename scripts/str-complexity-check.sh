#!/usr/bin/env bash
# Measure the growth order of `Str` methods and string operators empirically.
#
# Each case is timed inside mutsu (`now`) at N and at 2N and the ratio of the
# two times is printed:
#
#   ratio ~2  -> linear in N
#   ratio ~4  -> quadratic in N
#
# Cases flagged SUPERLINEAR (ratio >= 3) correspond to `Rakudo: O(..) -- see
# #NNNN` annotations in the method implementations; the rules for those
# annotations are in docs/complexity-annotations.md. Every case is linear in
# Rakudo (MUTSU_BIN=raku runs the same file; rakudo's absolute times are
# tiny, so its ratios are noise-dominated).
#
# A manual diagnostic, NOT a CI gate: wall-clock ratios move with load. Use a
# release build and re-run a case before trusting it.
#
# usage: scripts/str-complexity-check.sh [case-name-substring ...]
#   MUTSU_BIN   binary to measure (default target/release/mutsu)
#   SCALE       multiply every case's base N (default 1)

set -u
BIN=${MUTSU_BIN:-target/release/mutsu}
SCALE=${SCALE:-1}
FILTERS=("$@")

# name | base N | setup | timed body | mode
#   NN in setup/body is replaced by the size. mode "loop" (default) runs the
#   body as `for ^NN -> $i { body }`; mode "once" runs it once.
CASES=(
    # --- position / length -------------------------------------------------
    'chars (non-ASCII)|10000|my $s = "あ" x NN;|$s.chars|loop'
    'substr (non-ASCII)|10000|my $s = "あ" x NN;|$s.substr($i, 1)|loop'
    'substr (ASCII)|20000|my $s = "a" x NN;|$s.substr($i, 1)|loop'
    'index with pos|10000|my $s = "あ" x NN;|$s.index("あ", $i)|loop'
    'rindex with pos|10000|my $s = "あ" x NN;|$s.rindex("あ", $i)|loop'
    'contains with pos|10000|my $s = "あ" x NN;|$s.contains("あ", $i)|loop'
    'substr-eq|20000|my $s = "あ" x NN;|$s.substr-eq("あ", $i)|loop'
    'starts-with|50000|my $s = "あ" x NN;|$s.starts-with("あ")|loop'
    'ord|50000|my $s = "あ" x NN;|$s.ord|loop'
    'indices (one call)|10000|my $s = "a" x NN;|$s.indices("a")|once'
    'eq (different lengths)|50000|my $s = "a" x NN; my $o = "b" ~ $s;|$s eq $o|loop'
    'chomp (nothing to chomp)|50000|my $s = "a" x NN;|$s.chomp|loop'
    # --- building strings --------------------------------------------------
    'append ~= (plain local, control)|20000|my $x = "";|$x ~= "あ"|loop'
    'append ~= (stmt modifier, control)|10000|my $w = "";|$w ~= "あ" if True|loop'
    'append ~= (hash element)|10000|my %h = k => "";|%h<k> ~= "あ"|loop'
    'append ~= (in given/when)|10000||given 1 { when 1 { my $x = ""; for ^NN { $x ~= "あ" } } }|once'
    'concat reassign $y = $y ~|10000|my $y = "";|$y = $y ~ "あ"|loop'
    # --- transform ---------------------------------------------------------
    'trans (multi-char key)|10000|my $s = "abc " x NN;|$s.trans(["ab"] => ["x"])|once'
    'trans ("\n" => "\r\n")|10000|my $s = "abc\n" x NN;|$s.trans("\n" => "\r\n")|once'
    'trans (regex key)|5000|my $s = "abc " x NN;|$s.trans(/b/ => "x")|once'
    'trans (char ranges, control)|200000|my $s = "abc " x NN;|$s.trans("a..z" => "A..Z")|once'
    'Str.Str|20000|my $s = "a" x NN;|$s.Str|loop'
    'Str.WHICH|20000|my $s = "a" x NN;|$s.WHICH|loop'
    # --- split / match / subst ---------------------------------------------
    's:g/// (one call)|2000|my $c = "a," x NN;|$c ~~ s:g/","/;/|once'
    'subst regex + closure|1000|my $s = "a" x NN;|$s.subst(/a+/, { "b" })|once'
    'subst literal :x(*)|10000|my $s = "a," x NN;|$s.subst(",", ";", :x(*))|once'
    'split, two separators|10000|my $s = "a," x NN;|$s.split([",", ";"])|once'
    'comb(regex, :match)|2500|my $s = "a," x NN;|$s.comb(/a/, :match)|once'
    'match :p loop|10000|my $s = "a," x NN; my $p = 0;|while $s.match(/.","/, :p($p)) -> $m { $p = $m.to }|once'
    'Str ~~ /rx/ (per-call setup)|10000|my $s = "a" x NN;|$s ~~ /b/|loop'
    'split (string, control)|200000|my $s = "a," x NN;|$s.split(",")|once'
    'subst :g fast path (control)|200000|my $s = "a," x NN;|$s.subst(/","/, ";", :g)|once'
)

time_case() {
    local setup=$1 body=$2 mode=$3 n=$4
    setup=${setup//NN/$n}
    body=${body//NN/$n}
    local timed
    if [ "$mode" = once ]; then
        timed="$body;"
    else
        timed="for ^$n -> \$i { $body };"
    fi
    timeout 300 "$BIN" -e "$setup my \$t0 = now; $timed say now - \$t0;" 2>/dev/null | tail -1
}

printf '%-32s %8s %10s %10s %s\n' case N 't(N)' 't(2N)' ratio
for c in "${CASES[@]}"; do
    IFS='|' read -r name base setup body mode <<<"$c"
    if [ ${#FILTERS[@]} -gt 0 ]; then
        hit=0
        for f in "${FILTERS[@]}"; do
            [[ $name == *"$f"* ]] && hit=1
        done
        [ $hit = 1 ] || continue
    fi
    n=$((base * SCALE))
    t1=$(time_case "$setup" "$body" "${mode:-loop}" "$n")
    t2=$(time_case "$setup" "$body" "${mode:-loop}" $((n * 2)))
    verdict=$(awk -v a="$t1" -v b="$t2" 'BEGIN {
        if (a == "" || b == "" || a + 0 <= 0) { print "? (error/timeout)"; exit }
        r = b / a; printf "%.2f %s", r, (r >= 3 ? "SUPERLINEAR" : "ok") }')
    printf '%-32s %8d %10.4f %10.4f %s\n' "$name" "$n" "${t1:-0}" "${t2:-0}" "$verdict"
done
