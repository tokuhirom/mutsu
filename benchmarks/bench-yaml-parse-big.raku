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
# No raku baseline: YAMLish is a bundled battery, not installed for the system
# rakudo, so the ratio column records NA exactly as it does for
# bench-yaml-parse. The absolute and deterministic series are the signal here.
use YAMLish;

my $ROWS = 60;
my $text = "---\n"
    ~ (1..$ROWS).map({ "key$_: 'value $_ padded   here'\n" }).join
    ~ "...\n";

my $doc = load-yaml($text);
die "parse failed" unless $doc.elems == $ROWS;
say "yaml-parse-big: {$doc.elems} keys, {$text.chars} chars";
