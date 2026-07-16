use v6;
use Test;

plan 8;

# dir returns a Seq (Rakudo: dir.^name eq 'Seq'), so it flattens as a listop
# argument.
is dir.^name, 'Seq', 'dir returns a Seq';
ok (map { .relative }, dir).elems > 0, 'map over dir iterates its entries';
ok (map { .relative }, dir).all ~~ Str, 'each mapped dir entry is a relative Str';

# A Seq flattens as a map/grep list argument even from a scalar $ source.
# (a fresh Seq per use — a Seq is single-shot)
my $sm = (1, 2, 3).Seq;
is (map { $_ + 1 }, $sm).join(','), '2,3,4', 'map over a Seq in a $ var flattens';
my $sg = (1, 2, 3).Seq;
is (grep { $_ > 1 }, $sg).join(','), '2,3', 'grep over a Seq in a $ var flattens';

# IO::Handle.gist vs .Str
my $tmp = 'tmp/dir-seq-map-gist-handle.txt';
my $fh = open $tmp, :w;
ok $fh.gist.starts-with('IO::Handle<'), 'open handle .gist starts with IO::Handle<';
ok $fh.gist.ends-with('(opened)'), 'open handle .gist ends with (opened)';
$fh.close;
ok $fh.gist.ends-with('(closed)'), 'closed handle .gist ends with (closed)';
unlink $tmp;
