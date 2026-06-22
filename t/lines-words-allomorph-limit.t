use Test;

# `.lines($limit)` / `.words($limit)` accept any Numeric as the limit, including
# allomorphs (`<3>`, `<3e0>`, `<3+0i>`) — they coerce to a row count rather than
# being silently ignored (which used to read the whole file).

plan 12;

my $file = $*TMPDIR.add("mutsu-lines-{$*PID}.txt");
$file.spurt("one\ntwo\nthree\nfour\nfive\n");

# plain numerics
is $file.lines(3).elems,    3, 'Int limit';
is $file.lines(3e0).elems,  3, 'Num limit';
is $file.lines(3/1).elems,  3, 'Rat limit';

# allomorphs
is $file.lines(<3>).elems,    3, 'IntStr limit';
is $file.lines(<3e0>).elems,  3, 'NumStr limit';
is $file.lines(<3/1>).elems,  3, 'RatStr (eval) limit';
is $file.lines(<3+0i>).elems, 3, 'ComplexStr limit (zero imaginary)';

# no limit reads everything
is $file.lines.elems, 5, 'no limit reads all lines';

# .words with allomorph limit
my $wfile = $*TMPDIR.add("mutsu-words-{$*PID}.txt");
$wfile.spurt("a b c d e f");
is $wfile.words(<2>).elems, 2, 'IntStr words limit';
is $wfile.words(4).elems,   4, 'Int words limit';
is $wfile.words.elems,      6, 'no limit reads all words';

# limit larger than available is fine
is $file.lines(<100>).elems, 5, 'limit beyond EOF returns all';

$file.unlink;
$wfile.unlink;
