use Test;

# VM-native text output on a File+UTF8 IO::Handle (print / put / say / print-nl)
# — the write touches only handle state (③ native IO PR-D Tier-2a). Must behave
# identically to the interpreter's native fork. Stdout/Stderr and non-UTF8 File
# targets fall through to the interpreter and are exercised here too.

plan 12;

my $path = $*TMPDIR.child("mutsu-io-t2a-{$*PID}.txt");

# print (no newline) + numeric stringification + say + put
{
    my $fh = $path.open(:w);
    $fh.print("a");
    $fh.print(42);
    $fh.say("hello");
    $fh.put("world");
    $fh.close;
    is $path.slurp, "a42hello\nworld\n", 'print/say/put write the expected bytes';
}

# multiple args + list gist + print-nl
{
    my $fh = $path.open(:w);
    $fh.say(1, 2, 3);
    $fh.print-nl;
    $fh.say([4, 5, 6]);
    $fh.close;
    is $path.slurp, "123\n\n[4 5 6]\n", 'multi-arg say, print-nl and list gist';
}

# a custom nl-out terminator is honored by say/put/print-nl
{
    my $fh = $path.open(:w);
    $fh.nl-out = "|";
    $fh.say("x");
    $fh.put("y");
    $fh.print-nl;
    $fh.close;
    is $path.slurp, "x|y||", '.nl-out terminator is used for say/put/print-nl';
}

# return values are True
{
    my $fh = $path.open(:w);
    ok $fh.print("p") === True, 'print returns True';
    ok $fh.say("s") === True, 'say returns True';
    ok $fh.put("u") === True, 'put returns True';
    ok $fh.print-nl === True, 'print-nl returns True';
    $fh.close;
}

# :out-buffer is honored on the native write path
{
    my $fh = $path.open(:w);
    $fh.out-buffer = 4;
    $fh.print("ab");      # buffered
    $fh.print("cd");      # would overflow -> flush first
    $fh.close;            # flushes the rest
    is $path.slurp, "abcd", 'buffered native writes flush correctly';
}

# writing to a closed handle fails
{
    my $fh = $path.open(:w);
    $fh.close;
    dies-ok { $fh.say("nope") }, 'say on a closed handle dies';
}

# non-UTF8 File encoding still works (falls through to encode_with_encoding)
{
    my $lp = $*TMPDIR.child("mutsu-io-t2a-latin1-{$*PID}.txt");
    my $fh = $lp.open(:w);
    $fh.encoding('latin1');
    $fh.say("é");          # 1 byte in latin1 + newline
    $fh.close;
    is $lp.slurp(:bin).elems, 2, 'latin1 say writes encoded bytes (fall through)';
    $lp.unlink;
}

# Stdout/Stderr still route through emit_output (fall through, not broken)
{
    lives-ok { $*OUT.print("") }, '$*OUT.print still works';
    lives-ok { $*ERR.print("") }, '$*ERR.print still works';
}

$path.unlink;
