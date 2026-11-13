use Test;
# `use NativeCall` imports routines, which turns a call to a built-in such as
# `dir` into a lexical-import call site. Such a site passes `:name($var)` as a
# Pair holding `$var`'s container (for a possible `is rw` named parameter);
# the built-in must still see the plain value. The Readline distribution's
# library probe (`dir($path, :test($library-match))`) found nothing before.
use NativeCall;

plan 5;

my $dir = $*TMPDIR.add("mutsu-named-arg-container-$*PID");
$dir.mkdir;
LEAVE { .unlink for $dir.dir; $dir.rmdir }
$dir.add($_).spurt('') for <libfoo.so.7 libfoo.so.8 other.txt>;

my $re = rx/ 'libfoo.so.' (\d+) $ /;
is dir($dir, :test($re)).map(*.basename).sort.join(','), 'libfoo.so.7,libfoo.so.8',
    'dir(:test($regex-var)) filters by the regex';

my $str-test = 'other.txt';
is dir($dir, :test($str-test)).map(*.basename).join(','), 'other.txt',
    'dir(:test($str-var)) smart-matches the string';

my &pred = *.ends-with('.txt');
is dir($dir, :test(&pred)).map(*.basename).join(','), 'other.txt',
    'dir(:test(&code-var)) calls the predicate';

sub bump(:$n! is rw) { $n++ }
my $count = 1;
bump(:n($count));
is $count, 2, 'a user sub still gets the is-rw named container';

my $sep = '-';
is join($sep, <a b>), 'a-b', 'a positional variable argument is unaffected';
