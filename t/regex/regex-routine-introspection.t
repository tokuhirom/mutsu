use Test;

plan 29;

# A Raku `Regex` is a `Method`, so it answers the `Routine` introspection
# surface -- and never with just the parameters the declarator wrote: every
# regex signature carries the cursor invocant (`Mu $::`) and the implicit
# `*%_` that every method signature gets. So a bare `rx/a/`, which declares
# nothing at all, still has a two-element signature.
# https://github.com/tokuhirom/mutsu/issues/8318
#
# Every expectation below is rakudo's own answer, checked with `raku -e`.

# A parameterless literal, in both spellings.
is rx/a/.signature.raku, ':(Mu $:: *%_)', 'rx// signature is the bare method shape';
is rx/a/.arity, 1, 'rx// arity counts the invocant';
is rx/a/.count, 1, 'rx// count counts the invocant';
is /a/.signature.raku, ':(Mu $:: *%_)', '// signature is the bare method shape';
is /a/.arity, 1, '// arity counts the invocant';

# An adverb-carrying literal is the same value shape by a different internal
# representation; it must not fall off the dispatch.
is rx:i/a/.signature.raku, ':(Mu $:: *%_)', 'an adverbed rx// answers too';

# An anonymous declarator's own parameters sit between the two synthesized
# ones (they ride on the value itself -- #8293).
my $t = token ($x) { $x \d+ };
is $t.signature.raku, ':(Mu $:: $x, *%_)', 'an anonymous token reports its parameter';
is $t.arity, 2, '... and counts it in arity';
is $t.count, 2, '... and in count';

my $r = regex ($a, $b) { \d+ };
is $r.signature.raku, ':(Mu $:: $a, $b, *%_)', 'two parameters, in order';
is $r.arity, 3, 'arity is invocant + both';

# The synthesized `*%_` is suppressed by an explicit named slurpy, exactly as
# it is for a method declaration.
my $s = token (*%opts) { \d+ };
is $s.signature.raku, ':(Mu $:: *%opts)', 'an explicit named slurpy replaces *%_';
is $s.count, 1, '... and a named slurpy is not positional';

# Optionality and slurpiness are read off the real parameters, so arity and
# count come apart the way they do for any routine.
my $o = token ($x, $y?) { \d+ };
is $o.signature.raku, ':(Mu $:: $x, $y?, *%_)', 'an optional parameter renders as optional';
is $o.arity, 2, 'an optional parameter is not required';
is $o.count, 3, '... but is acceptable';

my $sl = token (*@rest) { \d+ };
is $sl.signature.raku, ':(Mu $:: *@rest, *%_)', 'a positional slurpy renders as slurpy';
is $sl.count, Inf, '... and makes count unbounded';

# A type constraint survives into the reported signature.
is (token (Int $n) { \d+ }).signature.raku, ':(Mu $:: Int $n, *%_)', 'a typed parameter keeps its type';

# The invocant slot is a real invocant Parameter, and a regex declares no
# return type.
is rx/a/.signature.params[0].invocant, True, 'params[0] is the invocant';
is rx/a/.signature.returns.raku, 'Mu', 'a regex declares no return type';

# A callable materializes one Signature object per declaration. The cache key
# owns the regex payload allocation, so it cannot alias a later declaration
# when allocator addresses are reused under MUTSU_GC=on.
ok &say.signature === &say.signature, 'a name-based Routine handle keeps its Signature identity';
my $stable = token ($x) { \d+ };
ok $stable.signature === $stable.signature, 'an anonymous token keeps its Signature identity';
my $same_a = token ($x) { \d+ };
my $same_b = token ($x) { \d+ };
ok $same_a.signature !=== $same_b.signature, 'same-text token declarations have distinct Signature identities';
my $plain = rx/a/;
ok $plain.signature === $plain.signature, 'a plain rx value keeps its Signature identity';
my $adverbed = rx:i/a/;
ok $adverbed.signature === $adverbed.signature, 'an adverbed rx value keeps its Signature identity';
my $literal = /a/;
ok $literal.signature === $literal.signature, 'a slash regex value keeps its Signature identity';

# A grammar `token` reached through the MOP is a `Regex` too, and its
# invocant is constrained to the declaring grammar. Its parameters live on
# the `token_defs` entry rather than on a value, so this is a second entry
# point into the same synthesis.
grammar G {
    token foo ($x) { \d+ }
    token bare     { \d+ }
}
is G.^lookup('foo').signature.raku, ':(G $:: $x, *%_)', 'a grammar token reports its parameter';
is G.^lookup('bare').signature.raku, ':(G $:: *%_)', '... and a parameterless one the bare shape';
