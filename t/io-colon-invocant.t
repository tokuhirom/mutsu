use Test;

plan 4;

my $tmpfile = "_tmp-io-colon-invocant-{$*PID}-{100000.rand.Int}.txt";

{
    my $fh = open($tmpfile, :w);
    say $fh: "alpha";
    print $fh: "beta";
    put $fh: "gamma";
    close $fh;
}

my $content = slurp $tmpfile;
my @lines = $content.lines;

is +@lines, 2, "handle colon-invocant writes two lines";
is @lines[0], "alpha", "say with handle invocant writes first line";
is @lines[1], "betagamma", "print+put with handle invocant share second line content";
ok unlink($tmpfile), "cleanup temp file";
