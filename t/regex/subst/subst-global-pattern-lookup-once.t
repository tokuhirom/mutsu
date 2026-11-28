# `.subst(rx, ..., :g)` finds every occurrence first and evaluates the
# replacements afterwards, so a static pattern is looked up once for the whole
# walk (as `split` does) instead of once per occurrence. A pattern that
# interpolates a variable still reads it. Values are rakudo's.
use Test;

plan 10;

my $s = 'user1=a user22=b user333=c';

is $s.subst(/ 'user' (\d+) /, { 'u' ~ $0 }, :g), 'u1=a u22=b u333=c',
    'a closure replacement sees each occurrence\'s capture';
is $s.subst(/ \d+ /, '#', :g), 'user#=a user#=b user#=c', 'a string replacement';
is $s.subst(/ \d+ /, '#', :x(2)), 'user#=a user#=b user333=c', ':x(2) stops after two';
is $s.subst(/ \d+ /, '#', :nth(2)), 'user1=a user#=b user333=c', ':nth(2) picks one';
is $s.subst(/ <?after 'user'> \d+ /, { $/.from }, :g), 'user4=a user12=b user21=c',
    'each Match keeps its position in the subject';
is 'aaa'.subst(/ a* /, 'X', :g), 'XX', 'an empty match after the run still advances';

{
    my $x = 'b';
    is 'abcabc'.subst(/ $x /, 'X', :g), 'aXcaXc', 'an interpolated variable is read';
    my $n = 0;
    is 'a1b2c3'.subst(/ \d /, { ++$n }, :g), 'a1b2c3', 'a closure counter runs per occurrence';
    is $n, 3, 'once per occurrence';
}

# The work: 200 occurrences of a static pattern cost a handful of parse-cache
# lookups, not one per occurrence.
{
    my $script = q:to/END/;
        my $text = (^200).map({ "w$_" }).join(' ');
        my $r = $text.subst(/ \d+ /, { $/.Str.chars }, :g);
        say $r.chars;
        END
    my %env = %*ENV;
    %env<MUTSU_VM_STATS> = '1';
    my $proc = run($*EXECUTABLE, '-e', $script, :out, :err, :%env);
    $proc.out.slurp(:close);
    my $line = $proc.err.slurp(:close).lines.first(*.contains('regex-parse-cache:')) // '';
    my $hits = +(($line ~~ / 'hits=' (\d+) /)[0] // -1);
    ok 0 <= $hits < 50, "one pattern lookup per walk ($hits cache hits for 200 occurrences)";
}
