use Test;

plan 2;

my $file = $*TMPDIR.add("mutsu-io-words-seek-{$*PID}.txt");
$file.spurt("alpha beta\ngamma\ndelta epsilon zeta\n");

my $fh = $file.open;
$fh.get;
$fh.get;
is $fh.words(2).join('|'), 'delta|epsilon',
    'limited words reads the requested prefix';

$fh.seek(0, SeekFromBeginning);
is $fh.words.join('|'), 'alpha|beta|gamma|delta|epsilon|zeta',
    'seek discards words left buffered from the old position';

$fh.close;
$file.unlink;
