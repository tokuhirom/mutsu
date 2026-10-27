use Test;

plan 5;

# `print`/`put`/`say` are `$*OUT.print(...)`: an exception thrown by a user
# `$*OUT.print` reaches the caller, and the text is NOT also written to the real
# stream. Red installs such a handle to forbid output inside a `map` block.
my @seen;

sub with-out(&code) {
    my $*OUT = class :: {
        method print(|c) { @seen.push("print:" ~ c.join); die "no output here" }
    };
    my $r = try { code(); "survived" };
    my $msg = $r // ("died: " ~ $!.message);
    $msg
}

is with-out({ print "x" }), 'died: no output here', 'print propagates the handle\'s exception';
is with-out({ say "y" }), 'died: no output here', 'say propagates it too';
is with-out({ put "z" }), 'died: no output here', 'and put';
is-deeply @seen, ["print:x", "print:y\n", "print:z\n"], 'the handle saw each write exactly once';

# A handle that merely collects output still works.
my $buf = '';
{
    my $*OUT = class :: { method print(*@a) { $buf ~= @a.join; True } };
    print "a"; say "b"; put "c";
}
is $buf, "ab\nc\n", 'a collecting handle receives the text';
