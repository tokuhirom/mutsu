use Test;
plan 18;

# `split` on `Str`/`Cool`/`IO::Path` and `fmt` on `Cool` are method-table rows
# (ADR-11276 §9.76). Expectations checked against raku.
is-deeply "a,b,,c".split(","), ("a", "b", "", "c").Seq, 'Str.split';
is-deeply "a,b,,c".split(",", :skip-empty), ("a", "b", "c").Seq, ':skip-empty';
is-deeply "a,b,c".split(",", 2), ("a", "b,c").Seq, 'a limit';
is-deeply "a,b,c".split(",", *), ("a", "b", "c").Seq, 'a Whatever limit';
is-deeply "a,b,c".split(",", "2"), ("a", "b,c").Seq, 'a string limit';
is-deeply "a1b2c".split(/\d/), ("a", "b", "c").Seq, 'a regex splitter';
is-deeply "a-b_c".split(<- _>), ("a", "b", "c").Seq, 'a list of splitters';
is-deeply 12321.split(2), ("1", "3", "1").Seq, 'Cool.split';
is-deeply (1, 2, 3).split(" "), ("1", "2", "3").Seq, 'List.split stringifies';

# IO::Path.split splits the file's contents, not the path.
my $file = $*TMPDIR.add("mutsu-split-fmt-rows-{$*PID}.txt");
$file.spurt("a,b\nc");
is-deeply $file.split(","), ("a", "b\nc").Seq, 'IO::Path.split';
is-deeply $file.split(/\n/), ("a,b", "c").Seq, 'IO::Path.split with a regex';
is-deeply $file.split(",", :skip-empty), ("a", "b\nc").Seq, 'IO::Path.split with an adverb';
$file.unlink;

is 42.fmt, "42", 'Cool.fmt';
is 42.fmt("%05d"), "00042", 'Cool.fmt with a format';
is "a".fmt("<%s>"), "<a>", 'Str.fmt';
is 3.5.fmt("%.2f"), "3.50", 'Rat.fmt';
is True.fmt("%s"), "True", 'Bool.fmt';
is (1, 2).fmt("%02d"), "01 02", 'List.fmt still answers';
