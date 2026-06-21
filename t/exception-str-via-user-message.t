use Test;

# `Exception.Str`/`.gist` delegate to `self.message` (raku semantics). When a user
# exception supplies `message` as a *method* — e.g. composed from a parameterized
# role `role X[Str $e] is Exception { method message {...} }` — rather than as an
# attribute, the native exception Str/gist fast path used to read the (absent)
# `message` attribute and render "X::Foo with no message". It must call the user
# method instead. (Template::Mustache defines all its exceptions this way.)

plan 5;

role X[Str:D $err] is Exception {
    has $.str;
    method message { "$err <$!str>" }
}
class X::FieldNotFound does X['Field not found'] { }

my $e = X::FieldNotFound.new(:str('missing'));

is $e.message, 'Field not found <missing>', '.message dispatches to the user method';
is $e.Str,     'Field not found <missing>', '.Str delegates to the user message method';
is $e.gist,    'Field not found <missing>', '.gist delegates to the user message method (no backtrace)';

# Caught (thrown) form: CATCH sees the same message.
my $caught;
{
    X::FieldNotFound.new(:str('boom')).throw;
    CATCH { default { $caught = .Str } }
}
is $caught, 'Field not found <boom>', 'a thrown user exception stringifies via its message method';

# A plain attribute-based message still works (no regression).
class X::Plain is Exception {
    has $.message;
}
is X::Plain.new(message => 'plain msg').Str, 'plain msg',
    'attribute-based message still works';
