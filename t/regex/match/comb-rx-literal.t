use Test;

plan 5;

# `rx//` (with or without adverbs) combs exactly like a bare `/.../`; it used
# to be searched for as its own stringified form and found nothing.
is-deeply q{ab}.comb(rx/a/), ('a',).Seq, 'rx/a/ literal';
my $r = rx/a/;
is-deeply q{xaya}.comb($r), ('a', 'a').Seq, 'rx// held in a variable';
is-deeply q{aA}.comb(rx:i/A/), ('a', 'A').Seq, 'rx:i// ignores case';
is-deeply q{a b}.comb(rx:s/a b/), ('a b',).Seq, 'rx:s// sigspace';

# The Syndicate::Discovery shape: a file-scope constant regex.
my constant $base-tag = rx:i/ '<base' <-[>]>* ['/>' | '>'] /;
is q{<html><base href="http://x/"></html>}.comb($base-tag)[0], '<base href="http://x/">',
    'a constant rx:i// regex combs its matches';
