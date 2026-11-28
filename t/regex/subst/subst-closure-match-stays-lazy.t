# A closure replacement in `.subst(rx, { ... }, :g)` runs once per occurrence
# with `$/`, `$0`.. and `$<name>` published. Publishing the named captures read
# `.hash` off the occurrence's lazy Match, which forced its whole attribute
# map even for a pattern with no named captures at all; such a pattern now
# skips the read. Values are rakudo's.
use Test;

plan 8;

my $s = 'user1=a user22=b';

is $s.subst(/ 'user' (\d+) /, { 'u' ~ $0 }, :g), 'u1=a u22=b', 'positional capture';
is $s.subst(/ 'user' $<n>=[\d+] /, { 'u' ~ $<n> }, :g), 'u1=a u22=b', 'named capture';
is $s.subst(/ 'user' $<n>=[\d+] /, { $/<n> ~ '!' }, :g), '1!=a 22!=b', 'named capture through $/';
is $s.subst(/ (\w) $<d>=[\d+] /, -> $m { $m<d> ~ $m[0] }, :g), 'use1r=a use22r=b',
    'a pointy block receives the Match';
is $s.subst(/ \d+ /, { $/.Str.chars }, :g), 'user1=a user2=b', '$/ is the occurrence';
is $s.subst(/ \d+ /, { $/.from }, :g), 'user4=a user12=b', 'its position';

# The work: 200 closure replacements on a pattern with no named captures
# materialize none of the occurrence Matches.
{
    my $script = q:to/END/;
        my $text = (^200).map({ "w$_" }).join(' ');
        my $r = $text.subst(/ 'w' (\d+) /, { 'x' ~ $0 }, :g);
        say $r.chars;
        END
    my %env = %*ENV;
    %env<MUTSU_VM_STATS> = '1';
    my $proc = run($*EXECUTABLE, '-e', $script, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $line = $proc.err.slurp(:close).lines.first(*.contains('match_materializations=')) // '';
    is $out.trim, '889', 'the replacement ran for every occurrence';
    my $mat = +(($line ~~ / 'match_materializations=' (\d+) /)[0] // -1);
    ok 0 <= $mat < 50, "the occurrence Matches stay lazy ($mat materializations for 200)";
}
