use Test;

# Lazy IO::CatHandle.lines / .handles backed by a live cat instance:
# mid-iteration changes to .chomp / .nl-in take effect, .handles is consumable
# concurrently, CR-LF is one line ending and normalizes to "\n", and a lazy
# `for $cat.lines` interleaves with the body so the cat stays mid-stream.

plan 9;

my $seq = 0;
sub tmpfile($content) {
    my $p = $*TMPDIR.add("mutsu-cat-{$*PID}-{$seq++}");
    $p.spurt: $content;
    $p
}

# --- lazy .lines reflects mid-iteration .chomp / .nl-in changes ---
{
    my $cat = IO::CatHandle.new: tmpfile("0\n1\r\n2Z\n3V4"), tmpfile(''), tmpfile("5\nZ6\r\n♥7♥");
    (my $lines = $cat.lines).cache;
    is-deeply $lines[^2], ("0", "1"), 'lazy lines: default chomp/nl-in';
    $cat.chomp = False;
    $cat.nl-in = [<Z V>];
    is-deeply $lines[2, 3], ("2Z", "\n3V"), 'lazy lines: nl-in change applies mid-stream';
    is-deeply $lines[4, 5], ("4", "5\nZ"), 'lazy lines: change carries to next handle';
    $cat.chomp = True;
    $cat.nl-in = '♥';
    is-deeply $lines[6, 7], ("6\n", "7"), 'lazy lines: CR-LF normalizes to \n';
    $cat.close;
}

# --- CR-LF is one line ending (longest-match separator) ---
{
    my $cat = IO::CatHandle.new: tmpfile("a\r\nb\r\nc");
    is-deeply $cat.lines, ("a", "b", "c"), 'CR-LF is a single line ending';
}

# --- lazy .handles is consumable concurrently with the cat ---
{
    my $cat := IO::CatHandle.new: tmpfile("a1\na2\na3\na4"), tmpfile("b1\nb2\nb3\nb4"), tmpfile("c1\nc2\nc3\nc4");
    is-deeply $cat.handles.map({ eager .lines: 2 }),
        (<a1 a2>, <b1 b2>, <c1 c2>).Seq, 'lazy .handles: reads 2 lines per handle';
}

# --- lazy `for $cat.lines` interleaves with the body (on-switch + .path) ---
{
    my $ln;
    my $cat = IO::CatHandle.new: :on-switch(-> { $ln = 1 }),
        tmpfile("a\nb\nc"), tmpfile("d\ne");
    my @nums = gather for $cat.lines { take $ln++ }
    is-deeply @nums, [1, 2, 3, 1, 2], 'lazy for: on-switch resets per file';
}

# --- readchars honours utf8-c8 (round-trips against Buf.decode) ---
{
    my $cat = IO::CatHandle.new: tmpfile('foo'), tmpfile(Buf.new(200));
    $cat.encoding: 'ascii';
    is $cat.readchars(3), 'foo', 'readchars: ascii prefix';
    $cat.encoding: 'utf8-c8';
    is-deeply $cat.readchars(1), Buf.new(200).decode('utf8-c8'),
        'readchars: utf8-c8 synthetic grapheme';
    $cat.close;
}
