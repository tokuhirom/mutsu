# The other sigil aliases on a `<$var>` call name the called regex's own
# Match, nested captures included, as the scalar `$<a>=<$re>` already did
# (#10673): an array alias `@<a>=<$re>`, a numbered alias `$0=<$re>`, and a
# Str-valued variable `$<a>=<$s>` / `<a=$s>`. Rakudo's `subrule_alias` renames
# the call under any sigil alias. Each case must print rakudo 2026.07's values.
use Test;

my $prelude = q:to/END/;
    my $r = /<digit>+/;
    my $s = '(\d)+';
    my regex d { <digit>+ }
    END

my @cases =
    '@<a>=<$re> is a List of the call\'s Match, captures nested',
        'my $m = "12" ~~ /@<rx>=<$r>/; say ($m<rx> ~~ Positional, $m<rx>.elems, ~$m<rx>[0], $m<rx>[0]<digit>.elems).join("|")',
        'True|1|12|2',
    '$0=<$re> holds the call\'s Match in slot 0',
        'my $m = "12" ~~ /$0=<$r>/; say ($m.list.elems, ~$m[0], $m[0]<digit>.elems, ~$m[0]<digit>[1]).join("|")',
        '1|12|2|2',
    '$0=<rule> nests the rule\'s captures, which also file under the rule',
        'my $m = "12" ~~ /$0=<d>/; say ($m[0]<digit>.elems, $m<d>.defined).join("|")',
        '2|True',
    '$<a>=<$s> matches a Str as a pattern, its group nested',
        'my $m = "12" ~~ /$<rx>=<$s>/; say (~$m<rx>, $m<rx>[0].elems, ~$m<rx>[0][1]).join("|")',
        '12|2|2',
    '<a=$s> is the same',
        'my $m = "12" ~~ /<rx=$s>/; say (~$m<rx>, $m<rx>[0].elems).join("|")',
        '12|2',
    '@<a>=<$re>+ lists the repetitions',
        'my $m = "12a" ~~ /@<n>=<$r>+ <alpha>/; say ($m<n>.elems, $m<n>[0]<digit>.elems).join("|")',
        '1|2',
    'the scalar $<a>=<$re> still nests (#10522)',
        'my $m = "12" ~~ /$<rx>=<$r>/; say ($m<rx><digit>.elems, $m<digit>.defined).join("|")',
        '2|False';

plan @cases / 3;

for @cases -> $name, $match, $expected {
    my $code = $prelude ~ $match;
    my $proc = run($*EXECUTABLE, '-e', $code, :out, :err);
    is $proc.out.slurp(:close).trim, $expected, $name;
    $proc.err.slurp(:close);
}
