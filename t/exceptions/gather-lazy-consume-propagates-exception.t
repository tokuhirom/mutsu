use Test;

# #8159: an exception thrown inside a `gather { ... }` body propagated when
# the resulting Seq was reified EAGERLY (`my @r = gather {...}; say @r`), but
# was silently swallowed -- printing an empty line and exiting 0 -- when the
# Seq was consumed LAZILY (`say gather {...}` directly).
#
# Root cause: `say`'s `.gist` dispatch on a LazyList forces the gather body
# and re-dispatches onto the resulting Seq. When the force itself threw
# `X::Method::NotFound` (as it does here, from the body's own `.nosuchmethod`
# call), the renderer mistook that for "no .gist candidate exists" -- the same
# exception type a genuinely gist-less type would raise -- and fell back to
# the native placeholder gist instead of propagating the user's exception.

plan 6;

throws-like 'say gather { "u".nosuchmethod }', X::Method::NotFound,
    method => 'nosuchmethod', typename => 'Str',
    'say gather {...} propagates a method-not-found thrown by the body';

throws-like 'my @r = gather { "u".nosuchmethod }; say @r', X::Method::NotFound,
    method => 'nosuchmethod', typename => 'Str',
    'eager reification into an array still propagates the same exception';

throws-like 'say gather { die "inner" }', X::AdHoc,
    'a plain die inside a lazily-consumed gather still propagates';

throws-like 'say gather { "u".take-rw }', X::Method::NotFound,
    method => 'take-rw', typename => 'Str',
    'take-rw on a value with no such method propagates too';

throws-like 'say gather { 1.take; "u".nope }', X::Method::NotFound,
    method => 'nope', typename => 'Str',
    'a throw after some values were already taken still propagates, not ()';

# A gather with no error still renders normally through the same lazy `say`
# path (pin against a fix that overcorrects and breaks the happy path).
is (gather { take 1; take 2; take 3 }).gist, '(1 2 3)',
    'a successful gather still gists its taken elements';
