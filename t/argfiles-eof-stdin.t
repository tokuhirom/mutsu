use Test;

plan 12;

# `$*ARGFILES.eof` must eventually report True so that
#     while !$*ARGFILES.eof { say $*ARGFILES.get }
# terminates. It used to be hardcoded to False for both the stdin and the
# ArgFiles handle targets, so the loop spun forever printing Nil.
#
# Rakudo's semantics, which these tests pin:
#   * a non-seekable source (stdin) reports eof only after a read actually hit
#     end-of-stream, so the loop sees exactly one trailing Nil;
#   * an `$*ARGFILES` over real files behaves like IO::CatHandle: eof advances
#     at most one file per call and never skips an empty file, so a non-empty
#     final file gives no trailing Nil while an empty final file gives one.

my $exe = $*EXECUTABLE;

# Drive `$*ARGFILES` with a bounded loop so a regression fails the test rather
# than hanging the suite.
my $loop = 'my $n = 0; while !$*ARGFILES.eof { say $*ARGFILES.get.raku; last if ++$n > 6 }; say "DONE"';

sub run-with-stdin($code, $stdin, *@args) {
    my $proc = run($exe, '-e', $code, |@args, :in, :out);
    $proc.in.print($stdin) if $stdin.chars;
    $proc.in.close;
    $proc.out.slurp(:close);
}

my $dir = $*TMPDIR.add("mutsu-argfiles-eof-{$*PID}");
$dir.mkdir;
my $one   = $dir.add('one.txt');
my $two   = $dir.add('two.txt');
my $empty = $dir.add('empty.txt');
$one.spurt("a\nb\n");
$two.spurt("c\n");
$empty.spurt('');

LEAVE {
    for $one, $two, $empty { .unlink if .e }
    $dir.rmdir if $dir.e;
}

# --- stdin fallback (no file arguments) ------------------------------------

is run-with-stdin($loop, ''),
   "Nil\nDONE\n",
   'no file args + empty stdin terminates after a single Nil';

is run-with-stdin($loop, "x\ny\n"),
   "\"x\"\n\"y\"\nNil\nDONE\n",
   'no file args + stdin data yields the lines then one trailing Nil';

is run-with-stdin('say $*ARGFILES.eof', ''),
   "False\n",
   '.eof is False before any read even when stdin is empty';

is run-with-stdin('$*ARGFILES.get; say $*ARGFILES.eof', ''),
   "True\n",
   '.eof flips to True once a read hit end-of-stream';

# --- real file arguments ----------------------------------------------------

is run-with-stdin($loop, '', ~$one),
   "\"a\"\n\"b\"\nDONE\n",
   'one file argument terminates right after the last line (no trailing Nil)';

is run-with-stdin($loop, '', ~$one, ~$two),
   "\"a\"\n\"b\"\n\"c\"\nDONE\n",
   'two file arguments are concatenated, then the loop stops';

is run-with-stdin('say $*ARGFILES.eof', '', ~$one),
   "False\n",
   '.eof is False before any read with a file argument';

is run-with-stdin('say $*ARGFILES.eof', '', ~$empty),
   "False\n",
   '.eof is False before any read even when the only file is empty';

is run-with-stdin($loop, '', ~$empty),
   "Nil\nDONE\n",
   'a single empty file yields one Nil, matching IO::CatHandle';

is run-with-stdin($loop, '', ~$empty, ~$two),
   "\"c\"\nDONE\n",
   'a leading empty file is skipped by .get without a spurious Nil';

is run-with-stdin($loop, '', ~$two, ~$empty),
   "\"c\"\nNil\nDONE\n",
   'a trailing empty file yields exactly one Nil';

# --- reading the files still works unchanged --------------------------------

is run-with-stdin('print $*ARGFILES.slurp', '', ~$one, ~$two),
   "a\nb\nc\n",
   '.slurp over several file arguments is unaffected';
