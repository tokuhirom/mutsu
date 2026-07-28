# YAML-parse benchmark: the bundled YAMLish battery over a block mapping whose
# values are block sequences of single-quoted scalars with embedded runs of
# spaces — the shape that exposed two real perf bugs (see
# todo/tickets/yaml-parse-throughput.md): a regex code-block writeback
# compared by Debug-formatting the whole env, and every match-tree node
# (including per-character space/bare-string tokens) paying full multi-dispatch
# resolution just to find the actions class has no method for it. Both are
# fixed, but this shape is exactly what a config/data file with padded columns
# looks like in practice, so it stays as the regression benchmark for this
# class of bug rather than only the flat `k$_: v$_` micro-case in the ticket.
use YAMLish;

my $ROWS = 3;
my $row = '      16G         05C        ';
my $section = ("  - '" ~ $row ~ "'\n") x $ROWS;

my $text = "---\n"
    ~ (1..2).map({ "section$_:\n" ~ $section }).join
    ~ "...\n";

my $doc = load-yaml($text);
die "parse failed" unless $doc<section1>.elems == $ROWS;
say "yaml-parse: {$doc.elems} sections, {$doc<section1>.elems} rows each";
