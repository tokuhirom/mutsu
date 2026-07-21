use Test;

# `prompt` returns the `Any` type object at end of input (like `get`), not an
# empty string, so `prompt(...).defined` is False. Regression pin for
# Language/py-nutshell.rakudoc doc-diff finding.

plan 4;

my $exe = $*EXECUTABLE;

# Drive a subprocess with empty stdin so prompt hits EOF immediately.
spurt "tmp/prompt-eof-prog.raku", 'my $x = prompt("p "); print $x.^name, "|", $x.defined, "|", $x.gist;';
my $out = qqx{echo -n "" | $exe tmp/prompt-eof-prog.raku};

# The prompt message "p " is written to stdout before the type-object output.
is $out, "p Any|False|(Any)", 'prompt at EOF returns the Any type object';

# Normal input still yields a Str.
spurt "tmp/prompt-line-prog.raku", 'my $x = prompt("q "); print $x.^name, "|", $x;';
my $out2 = qqx{echo "hello" | $exe tmp/prompt-line-prog.raku};
is $out2, "q Str|hello", 'prompt with input returns the read Str';

# get shares the EOF semantics.
spurt "tmp/get-eof-prog.raku", 'my $x = get(); print $x.^name, "|", $x.defined;';
my $out3 = qqx{echo -n "" | $exe tmp/get-eof-prog.raku};
is $out3, "Any|False", 'get at EOF returns the Any type object';

unlink $_ for <tmp/prompt-eof-prog.raku tmp/prompt-line-prog.raku tmp/get-eof-prog.raku>;
pass 'cleanup';
