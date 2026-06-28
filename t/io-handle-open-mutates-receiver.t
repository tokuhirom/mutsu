use Test;

# In Raku `$fh.open(...)` opens the handle *and returns self*, so a subsequent
# method call on the same binding operates on the now-opened handle. mutsu's
# `.open` builds a fresh opened-handle instance (the OS handle lives in the
# handle table); the receiver binding must be updated to reflect the open.
# Regression for roast S32-io/io-handle.t `.print-nl` (subtest 23).

plan 6;

my $dir = $*TMPDIR.add("mutsu-open-mutate-{$*PID}");
$dir.mkdir;
LEAVE { try $dir.&{ .dir».unlink; .rmdir } }

# Variable receiver: .open then .print on the same variable.
{
    my $path = $dir.add("a.txt");
    my $fh = IO::Handle.new(:path($path));
    $fh.open(:w);
    $fh.print("hello");
    $fh.close;
    is $path.slurp, "hello", '.open mutates a variable receiver (.print works after)';
}

# .nl-out passed to .open is honored by .print-nl.
{
    my $path = $dir.add("b.txt");
    my $fh = IO::Handle.new(:path($path));
    $fh.open(:w, :nl-out<bar>);
    $fh.print-nl;
    $fh.close;
    is $path.slurp, "bar", '.open(:nl-out) honored by .print-nl on the receiver';
}

# Re-open (write then append) on the same variable.
{
    my $path = $dir.add("c.txt");
    my $fh = IO::Handle.new(:path($path));
    $fh.open(:w, :nl-out<bar>);  $fh.print-nl;  $fh.close;
    $fh.open(:a);  $fh.nl-out = 'ber';  $fh.print-nl;  $fh.close;
    is $path.slurp, "barber", 're-open (append) on the same receiver';
}

# Topic receiver inside `with` (the exact roast shape).
{
    my $path = $dir.add("d.txt");
    with IO::Handle.new(:path($path)) {
        .open: :w, :nl-out<bar>;  .print-nl;  .close;
    }
    is $path.slurp, "bar", '.open mutates the `with` topic receiver';
}

# Temp (non-variable) receiver still works: result is the opened handle.
{
    my $path = $dir.add("e.txt");
    my $fh = IO::Handle.new(:path($path)).open(:w);
    $fh.print("temp");
    $fh.close;
    is $path.slurp, "temp", '.open on a temp receiver returns the opened handle';
}

# Opening a missing file for read returns a Failure and leaves the binding
# usable (does not corrupt the receiver into something un-droppable).
{
    my $path = $dir.add("missing.txt");
    my $fh = IO::Handle.new(:path($path));
    my $r = $fh.open(:r);
    ok $r ~~ Failure, '.open of a missing file for read returns a Failure';
}
