use Test;

plan 9;

# An Exception instance stringifies (.Str / .gist / ~ / interpolation) via its
# .message — including a user `class E is Exception` whose name is not X::* and
# whose message() is a method that interpolates attributes.

class X::HTTP is Exception {
    has $.url;
    has $.status;
    method message() { "Error to $.url: $.status" }
}

my $e = X::HTTP.new(url => 'http://x', status => 404);
is $e.message, 'Error to http://x: 404', '.message (user method) works';
is $e.Str,     'Error to http://x: 404', '.Str returns .message';
is $e.gist,    'Error to http://x: 404', '.gist returns .message';
is ~$e,        'Error to http://x: 404', 'prefix ~ returns .message';
is "$e",       'Error to http://x: 404', 'interpolation returns .message';

# A non-X:: user exception class is detected via its MRO.
class MyError is Exception { method message { 'boom' } }
is "{MyError.new}", 'boom', 'non-X:: exception detected via MRO';

# Built-in X::AdHoc (from `die "..."`) stringifies to its payload and renders
# `.raku` with the payload.
{
    my $caught;
    try { die "Neat error message" };
    $caught = $!;
    is "$caught", 'Neat error message', 'X::AdHoc interpolates its payload';
    is $caught.raku, 'X::AdHoc.new(payload => "Neat error message")',
        'X::AdHoc.raku shows payload';
}

# Typed built-in exception still stringifies (regression guard).
{
    try { "abc".substr(10) };
    ok "$!".chars > 0, 'typed built-in exception still stringifies';
}
