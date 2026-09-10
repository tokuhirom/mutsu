use Test;

plan 3;

# `slurp-rest` is the deprecated Rakudo spelling of "slurp the rest of the
# handle from the current position" (META6's
# `multi method new(IO::Handle :$file!)` uses it).
my $path = $*TMPDIR.add("mutsu-slurp-rest-{$*PID}.txt");
$path.spurt("hello\nworld\n");
END $path.unlink;

my $h = $path.open;
is $h.slurp-rest, "hello\nworld\n", 'slurp-rest reads the whole handle';
$h.close;

my $h2 = $path.open;
is $h2.get, "hello", 'get reads the first line';
is $h2.slurp-rest, "world\n", 'slurp-rest reads from the current position';
$h2.close;

done-testing;
