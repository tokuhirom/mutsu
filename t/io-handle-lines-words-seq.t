use Test;

plan 14;

my $path = "tmp/io-handle-lines-words-seq-$*PID.txt".IO;
spurt $path, "alpha beta\ngamma delta\n";

my $lines-handle = $path.open;
my $lines := $lines-handle.lines;
is $lines.WHAT, Seq, 'IO::Handle.lines returns a Seq';
is $lines-handle.tell, 0, '.WHAT does not consume lines';
is $lines.raku, '("alpha beta", "gamma delta").Seq',
    'lines retain Seq identity when materialized';
ok $lines-handle.eof, 'materializing lines consumes the handle';
$lines-handle.close;

my $lines-once-handle = $path.open;
my $lines-once := $lines-once-handle.lines;
is $lines-once.list.join('|'), 'alpha beta|gamma delta',
    'lines Seq can be consumed once';
throws-like { $lines-once.list }, X::Seq::Consumed,
    'lines Seq cannot be consumed twice';
$lines-once-handle.close;

my $words-handle = $path.open;
my $words := $words-handle.words;
is $words.WHAT, Seq, 'IO::Handle.words returns a Seq';
is $words.raku, '("alpha", "beta", "gamma", "delta").Seq',
    'words retain Seq identity when materialized';
ok $words-handle.eof, 'materializing words consumes the handle';
$words-handle.close;

my $words-once-handle = $path.open;
my $words-once := $words-once-handle.words;
is $words-once.list.join('|'), 'alpha|beta|gamma|delta',
    'words Seq can be consumed once';
throws-like { $words-once.list }, X::Seq::Consumed,
    'words Seq cannot be consumed twice';
$words-once-handle.close;

my $remainder-handle = $path.open;
is $remainder-handle.get, 'alpha beta', 'a handle can be partially consumed first';
my $remainder := $remainder-handle.lines;
is $remainder.WHAT, Seq, 'remaining lines are still returned as a Seq';
is $remainder.raku, '("gamma delta",).Seq',
    'lines lazily reads only the remainder of a partially consumed handle';
$remainder-handle.close;

unlink $path;
