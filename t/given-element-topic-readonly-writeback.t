use v6;
use Test;

# A `given`/`with` topic that is a *container element* (`given %h<k>` /
# `given @a[i]`) aliases that element read-write: reassigning `$_` in the body
# propagates back. But if the body never changes `$_`, the writeback must be a
# no-op. mutsu unconditionally re-stored `$_` into the element on block exit;
# when the container was a read-only aggregate (a grammar Match subcapture,
# `with $cc<scheme>` where `$cc` is a `Match`), that spurious write clobbered the
# whole `$cc`, so every other subcapture (`$cc<hier-part>`) turned into `Any`.
# This is the shape that broke URI's `parse` method.

plan 9;

# Read-only associative element topic on a Match subcapture must NOT corrupt it.
{
    grammar G {
        token TOP  { <uri> }
        token uri  { <scheme> '://' <rest> }
        token scheme { \w+ }
        token rest   { \S+ }
    }
    my $cc = G.parse('http://example.com/a')<uri>;
    my $scheme = '';
    $scheme = .lc with $cc<scheme>;
    is $scheme, 'http', 'with $cc<scheme> reads the subcapture';
    is $cc<scheme>.Str, 'http', '$cc<scheme> is still usable afterwards';
    is $cc<rest>.Str,   'example.com/a', 'the *other* subcapture is preserved';
    ok $cc.defined, '$cc itself is still a defined Match';
}

# The same, but inside a nested sub scope (the shape that actually broke): a
# grammar-parse subcapture passed on to a typed sub after a `with` on a sibling.
{
    grammar G2 {
        token TOP { <scheme> '://' <host> }
        token scheme { \w+ }
        token host   { \S+ }
    }
    sub recv(Match:D $c) { $c.Str }
    sub parse($str) {
        my $cc = G2.parse($str);
        my $s = '';
        $s = .lc with $cc<scheme>;
        return recv($cc<host>);
    }
    is parse('http://example.com'), 'example.com',
        'nested with $cc<k> leaves the sibling subcapture a valid Match:D';
}

# Writeback still happens when the body *does* reassign the topic (hash var).
{
    my %h = a => 1;
    given %h<a> { $_ = 5 }
    is %h<a>, 5, 'given %h<k> writes back a reassigned topic';
}

# Read-only body is a no-op for a hash element, and still binds $_.
{
    my %h = a => 1;
    my $seen;
    given %h<a> { $seen = $_ }
    is %h<a>, 1, 'read-only given %h<k> leaves the element unchanged';
    is $seen, 1, 'read-only given %h<k> still binds $_ to the element';
}

# Array element writeback still works.
{
    my @a = 1, 2, 3;
    given @a[1] { $_ = 9 }
    is-deeply @a, [1, 9, 3], 'given @a[i] writes back a reassigned topic';
}
