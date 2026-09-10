use Test;

plan 8;

my $file = 'tmp-io-get';
spurt $file, "a\n\nb\n";

my $fh = open($file);
is get($fh), "a", 'get strips trailing newline';
is get($fh), "", 'get preserves empty lines';
is get($fh), "b", 'get reads following lines';
ok get($fh) === Nil, 'get returns Nil at EOF';
$fh.close;

spurt $file, q:to/END/;
>first
abc
>second
xyz
END

$fh = open($file, nl-in => ["\n>", "\r\n>"]);
my @records;
while my $record = get $fh {
    @records.push($record);
}
is @records.elems, 2, 'nl-in splits records';
ok @records[0] ~~ /^'>first'/, 'first record keeps initial marker';
ok @records[1] ~~ /^'second'/, 'second record consumes separator marker';
$fh.close;

ok unlink($file), 'temporary file removed';
