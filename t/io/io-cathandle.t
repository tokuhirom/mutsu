use Test;

# Basic IO::CatHandle: read across several files in sequence.

my $dir = $*TMPDIR.add("mutsu-cathandle-{$*PID}");
$dir.mkdir;
LEAVE { try { .unlink for $dir.dir; $dir.rmdir } }

my $f1 = $dir.add("a.txt"); $f1.spurt("a\nb\nc\n");
my $f2 = $dir.add("b.txt"); $f2.spurt("d\ne\n");
my $f3 = $dir.add("c.txt"); $f3.spurt("");
my $f4 = $dir.add("d.txt"); $f4.spurt("f\n");

isa-ok IO::CatHandle.new, IO::CatHandle, 'IO::CatHandle.new';

{
    my $cat = IO::CatHandle.new: $f1, $f2;
    is $cat.get, 'a', 'get first line';
    is $cat.get, 'b', 'get second line';
    is $cat.get, 'c', 'get third line (still first file)';
    is $cat.get, 'd', 'get crosses to second file';
    is $cat.get, 'e', 'get last line';
    is-deeply $cat.get, Nil, 'get past EOF returns Nil';
}

{
    # Empty files in the middle are skipped transparently.
    my $cat = IO::CatHandle.new: $f1, $f3, $f4;
    is-deeply $cat.lines.List, ('a', 'b', 'c', 'f'), 'lines skips empty file';
}

{
    my $cat = IO::CatHandle.new: $f1, $f2;
    is $cat.slurp, "a\nb\nc\nd\ne\n", 'slurp concatenates all files';
}

{
    my $cat = IO::CatHandle.new: $f1, $f2;
    is $cat.opened, True, 'opened before close';
    is $cat.close, True, 'close returns True';
    is $cat.opened, False, 'not opened after close';
    is-deeply $cat.slurp, Nil, 'slurp after close returns Nil';
}

{
    # chomp / nl-in / encoding accessors.
    my $cat = IO::CatHandle.new: $f1;
    is $cat.encoding, 'utf8', 'default encoding';
    is $cat.chomp, True, 'default chomp';
}

done-testing;
