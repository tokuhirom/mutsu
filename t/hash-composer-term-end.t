use Test;

# A `{ ... }` body that opens with a pair is a hash composer unless it
# references the topic. Deciding that lexically means knowing where a term
# ends: `$/`, `$!` and a closing quote/regex delimiter all end one, so a
# `.method` after them has a real invocant and does NOT make the body a block.
# The unit tests in src/parser/primary/misc/lambda_tests.rs cover the decision
# directly; this file pins the end-to-end spellings, including the heredoc one
# (which needs its body on the following lines, so it cannot be a unit test).

plan 8;

'abc' ~~ /(\w+)/;

is { :pos($/.from) }.^name, 'Hash', '$/.from in a pair value keeps a hash composer';
is { :m($!.^name) }.^name, 'Hash', '$!.method in a pair value keeps a hash composer';
is { :a(q/x/.uc) }.^name, 'Hash', 'a closing q// delimiter ends a term';

my $h = { t => 'T', b => q:to/EOF/.trim, };
    line one
    line two
    EOF
is $h.^name, 'Hash', 'a heredoc with a trailing method call keeps a hash composer';
is $h<b>, "line one\nline two", 'the heredoc value survives';

my %outer = (x => { t => 'T', b => q:to/EOF/.trim, });
    nested one
    EOF
is %outer<x><b>, 'nested one', 'the same nested inside another literal';

# The one `/`-before-`.` spelling that really is a topic reference: infix
# division, which is written with a space on its left.
is { a => 1 / .elems }.^name, 'Block', 'infix division by a topic call is a block';
is { a => .key }.^name, 'Block', 'a bare invocant-less call is still a block';
