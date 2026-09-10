use v6;
use Test;

# A `given`/`with` topic that is a container element (`with $cc<key>` /
# `with $cc[i]`) reads the element from the container variable named in the
# topic. Under the per-store env-write gate, a plain lexical in a *nested* sub
# is authoritative in its local slot and its env mirror is suppressed — so the
# element-source read (`TagElementSource`), which looked the container up by
# name in the env, missed it and bound `$_` to `Any` (indexing a Nil container).
# This broke `with $cc<key>` on a grammar Match subcapture held in a nested
# sub's `my $cc` — the shape at the heart of the URI dist's URI.new parser.

plan 8;

grammar G {
    token TOP  { <uri> }
    token uri  { <auth> '/' <rest> }
    token auth { \w+ }
    token rest { \w+ }
}

# Associative element topic on a Match subcapture, inside a nested sub.
sub read-assoc($s) {
    my $cc = G.parse($s)<uri>;
    my $got;
    with $cc<auth> { $got = $_.Str }
    $got;
}
is read-assoc('foo/bar'), 'foo', 'nested with $cc<key> binds $_ to the subcapture';

# Pointy form (the exact URI shape: `with $x<k> -> $auth { ... }`) — the bound
# parameter must be a defined Match, not Any.
sub read-pointy($s) {
    my $cc = G.parse($s)<uri>;
    my $type = 'none';
    my $ok = False;
    with $cc<auth> -> $auth {
        $ok   = $auth.defined;
        $type = $auth.Str;
    }
    ($ok, $type);
}
{
    my ($ok, $type) = read-pointy('foo/bar');
    ok $ok, 'nested with $cc<key> -> $auth binds a *defined* Match';
    is $type, 'foo', 'the bound parameter carries the matched text';
}

# The bound Match must satisfy a `Match:D` parameter (URI passes it to
# `Authority.new(Match:D $auth)`).
sub recv(Match:D $c) { $c.Str }
sub feed($s) {
    my $cc = G.parse($s)<uri>;
    my $r;
    with $cc<auth> -> $auth { $r = recv($auth) }
    $r;
}
is feed('foo/bar'), 'foo', 'nested with-bound Match satisfies a Match:D sub param';

# Positional element topic on a Match subcapture, inside a nested sub.
grammar GP { token TOP { (\w+) '/' (\w+) } }
sub read-pos($s) {
    my $m = GP.parse($s);
    my $got;
    with $m[0] { $got = $_.Str }
    ($got, $m[1].Str);   # $m must still be usable afterwards
}
{
    my ($got, $rest) = read-pos('foo/bar');
    is $got, 'foo', 'nested with $m[i] binds $_ to the positional subcapture';
    is $rest, 'bar', 'the sibling positional subcapture is still usable';
}

# Regression: hash-var / array-var element writeback still works (env-backed,
# not gated locals), and a nested my-hash element reads correctly too.
sub hash-nested() {
    my %h = a => 1;
    my $seen;
    with %h<a> { $seen = $_ }
    ($seen, %h<a>);
}
{
    my ($seen, $v) = hash-nested();
    is $seen, 1, 'nested with %h<k> reads the element';
    is $v, 1, 'read-only nested with %h<k> leaves the element unchanged';
}
