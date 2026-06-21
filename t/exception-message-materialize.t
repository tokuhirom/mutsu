use Test;

# An exception whose `message` is a user *method* (e.g. composed from a
# parameterized role) must stringify to that message through EVERY stringification
# path, not just an explicit `.Str`/`.gist` call. mutsu's `join`/`sprintf "%s"`/
# `~`/interpolation go through the interpreter-less `to_string_value`, which cannot
# dispatch a user method — so the message is materialized into the `message`
# attribute once at construction. (Template::Mustache logs exceptions via
# `sprintf "%s", $e` and `@msgs.join`.)

plan 7;

role X[Str:D $err] is Exception {
    has $.str;
    method message { "$err <$!str>" }
}
class X::FieldNotFound does X['Field not found'] { }

my $e = X::FieldNotFound.new(:str('f1'));
my $expect = 'Field not found <f1>';

is $e.Str,                $expect, '.Str';
is ('' ~ $e),             $expect, 'concatenation (~)';
is "$e",                  $expect, 'string interpolation';
is ($e,).join,            $expect, '.join';
is sprintf('%s', $e),     $expect, 'sprintf %s';
is $e.message,            $expect, '.message (user method, unchanged)';

# A plain built-in-style exception with no user message method is unaffected:
# constructing it does not crash and it still reports its attribute message.
class X::Plain is Exception { has $.message; }
is sprintf('%s', X::Plain.new(message => 'plain')), 'plain',
    'attribute-message exception still stringifies normally';
