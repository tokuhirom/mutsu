use Test;

# `$h does R` on a variable holding an IO::CatHandle renames the instance's
# class to `IO::CatHandle+{R}` in place; the cat's native methods (and its
# read state across calls) must still answer it (#17755).

plan 4;

my $dir = $*TMPDIR.add("mutsu-cathandle-does-$*PID");
$dir.mkdir;
my $f1 = $dir.add('f1.txt');
my $f2 = $dir.add('f2.txt');
$f1.spurt("a\nb\n");
$f2.spurt("c\nd\n");

role Tagged { method tag { 'tagged' } }

my $h = IO::CatHandle.new($f1, $f2);
$h does Tagged;
is ($h.get, $h.get, $h.get).join(','), 'a,b,c', '.get keeps reading across calls and files';
is $h.tag, 'tagged', 'the role method answers too';
is $h.path.basename, 'f2.txt', '.path follows the active handle';

my $h2 = IO::CatHandle.new($f1, $f2);
$h2 does Tagged;
is $h2.lines.join(','), 'a,b,c,d', '.lines reads every file';

$f1.unlink; $f2.unlink; $dir.rmdir;
