use Test;

plan 5;

# `.wrap` on a built-in method found through `.^find_method` takes effect: the
# output-capture idiom (Terminal::MultiProgress's test helper) wraps
# `IO::Handle.print` and collects what `print` and `$*OUT.print` write.
my $text = '';
my $print = $*OUT.^find_method('print');
my $handle = $print.wrap: method (|c) { $text ~= c.list.join };
print 'a';
$*OUT.print('b', 'c');
start { print 'd' }.result;
$print.unwrap($handle);
is $text, 'abcd', 'print, $*OUT.print and a print in another thread go through the wrapper';

# Once unwrapped, output is no longer collected.
my $out = '';
{
    my $*OUT = class { method print(*@a) { $out ~= @a.join; True } }.new;
    print 'e';
}
is $out, 'e', 'a user $*OUT still receives print';
is $text, 'abcd', 'nothing more reached the removed wrapper';

# `callsame` from the wrapper reaches the native method.
my @seen;
my $h2 = $print.wrap: method (|c) { @seen.push: c.list.join; callsame };
{
    my $tmp = $*TMPDIR.add("mutsu-wrap-print-{$*PID}.txt");
    my $fh = $tmp.open(:w);
    $fh.print('xyz');
    $fh.close;
    is $tmp.slurp, 'xyz', 'callsame writes through the native print';
    $tmp.unlink;
}
$print.unwrap($h2);
is-deeply @seen, ['xyz'], 'the wrapper saw the call';

