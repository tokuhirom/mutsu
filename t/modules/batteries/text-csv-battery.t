use Test;

# Text::CSV is bundled (BATTERIES.md, docs/batteries/csv.md), so it must load
# and work with a plain `use` -- no `-I`, no install. This pins the zero-config
# resolution (including its Slang::Tuxic dependency activating at parse time)
# and one round-trip each for the method API and the functional csv() API; the
# exhaustive behaviour check is the release-time gate that runs the full
# upstream suite (scripts/battery-testsuite.sh).

plan 9;

use Text::CSV;

my $csv = Text::CSV.new;
ok $csv.parse('one,"two, with comma",three'), 'parse accepts a quoted line';
is-deeply [$csv.fields».text], ['one', 'two, with comma', 'three'],
    'parse splits quoted fields correctly';

ok $csv.combine('a', 'b,c', 'd'), 'combine accepts fields';
is $csv.string, 'a,"b,c",d', 'combine quotes the field with the separator';

# The bundled tree is upstream `main` past the 0.022 tag (#19210), which adds
# `column_indices` alongside `column_names`.
$csv.column_names(<a b c>);
isa-ok $csv.column_indices, Hash, 'column_indices (upstream after 0.022) is a Hash';

my @rows = csv(in => [['x', 'y'], ['1', '2']]);
is-deeply @rows, [['x', 'y'], ['1', '2']], 'csv(in => @data) round-trips rows';

my $file = $*TMPDIR.add("mutsu-text-csv-battery-$*PID.csv");
LEAVE $file.unlink if $file.e;
csv(in => [['h1', 'h2'], ['v1', 'v2']], out => $file.Str);
ok $file.e, 'csv(out => $file) writes a file';
is $file.slurp.lines[0], 'h1,h2', 'written CSV has the header line';

my @back = csv(in => $file.Str);
is-deeply @back, [['h1', 'h2'], ['v1', 'v2']], 'csv(in => $file) reads it back';
