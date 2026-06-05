use Test;

plan 3;

# `is` must force a lazy IO lines iterator before comparing, so it stringifies
# as its contents ("A B C") rather than the lazy gist "(...)".

my $path = "tmp/is-lazy-lines-{$*PID}.txt".IO;
LEAVE { try $path.unlink }

$path.spurt("A\nB\nC\n");
is $path.open(:r).lines, <A B C>, '.lines compares as its contents in `is`';

# Changing the input line separator (nl-in) and then reading .lines.
my $path2 = "tmp/is-lazy-lines2-{$*PID}.txt".IO;
LEAVE { try $path2.unlink }
$path2.spurt("A+B+C+D+");
my $fh = $path2.open(:r);
$fh.nl-in = "+";
is $fh.lines, <A B C D>, '.lines honors nl-in and compares as contents';

# is-deeply with the eager list form still works.
is-deeply $path.open(:r).lines.List, ("A", "B", "C"), '.lines.List is eager';
