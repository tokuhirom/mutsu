# Parsing a realistically-sized YAML document with the bundled YAMLish battery.
#
# `bench-yaml-parse.raku` parses a 6-row document (~0.07 s on CI) and is the
# regression guard for the two specific bugs its header names. It is NOT a guard
# for the thing #7576 is actually about -- how YAMLish's cost grows with document
# size -- because at 6 rows it cannot see growth at all.
#
# 60 rows, which is the size #7576's own measurements are quoted at. Locally
# (release, 2026-09-13), this shape scales:
#
#     30 rows  0.157 s     60 rows  0.310 s     90 rows  0.559 s
#    160 rows  1.154 s    640 rows 15.168 s
#
# Linear would be 0.147 s -> 3.1 s at 640 rows; it is 15.2 s, so there is still
# a superlinear term (~n^1.25 over this range, steepening at the top end) even
# after the 22 rounds of #7576. A document this size is completely ordinary --
# any CI config, any lockfile-shaped data -- so this is the series that says
# whether mutsu can read one.
#
# Raku baseline (#9916): YAMLish and its one dependency, MIME::Base64, are
# loaded from the vendored copies under modules/ through `use lib`, so rakudo
# runs the identical module source and the ratio column is real. (It recorded
# NA until 2026-09-30, when the system rakudo had no YAMLish to load.)
#
# WARM COST: two untimed loads, then a timed one printed as
# `bench-section-seconds:` -- see bench-regex-match.raku's header.
use lib $?FILE.IO.parent(2).add('modules/YAMLish/lib').Str;
use lib $?FILE.IO.parent(2).add('modules/MIME-Base64/lib').Str;
use YAMLish;

my $ROWS = 60;
my $text = "---\n"
    ~ (1..$ROWS).map({ "key$_: 'value $_ padded   here'\n" }).join
    ~ "...\n";

my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
load-yaml($text) for ^$warm;
my $t0 = now;
my $doc = load-yaml($text);
say "bench-section-seconds: {now - $t0}";
die "parse failed" unless $doc.elems == $ROWS;
say "yaml-parse-big: {$doc.elems} keys, {$text.chars} chars";
