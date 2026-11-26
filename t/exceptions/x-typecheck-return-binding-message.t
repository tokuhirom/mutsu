use Test;

# A hand-built `X::TypeCheck::Return` / `X::TypeCheck::Binding` words its own
# message, as rakudo's subclasses do. rakudo 2026.11's NativeCall throws
# `X::TypeCheck::Return.new(:got, :expected)` for a `--> T:D` native whose
# result fails the constraint (#19211).

plan 7;

is X::TypeCheck::Return.new(:got(Str), :expected(Str:D)).message,
    'Type check failed for return value; expected Str:D but got Str (Str)',
    'X::TypeCheck::Return.message with a type object';

is X::TypeCheck::Return.new(:got(42), :expected(Str)).message,
    'Type check failed for return value; expected Str but got Int (42)',
    'X::TypeCheck::Return.message with a value';

is X::TypeCheck::Binding.new(:got(42), :expected(Str)).message,
    'Type check failed in binding; expected Str but got Int (42)',
    'X::TypeCheck::Binding.message';

is X::TypeCheck.new(:got(42), :expected(Str), :operation<foo>).message,
    'Type check failed in foo; expected Str but got Int (42)',
    'X::TypeCheck.message still uses its :operation';

throws-like { X::TypeCheck::Return.new(:got(42), :expected(Str)).throw },
    X::TypeCheck::Return,
    message => 'Type check failed for return value; expected Str but got Int (42)',
    'a thrown hand-built X::TypeCheck::Return carries the message';

my $e = X::TypeCheck::Return.new(:got(42), :expected(Str));
is ~$e, 'Type check failed for return value; expected Str but got Int (42)',
    'Str renders the message';

sub f(--> Int:D) { Int }
throws-like { f() }, X::TypeCheck::Return,
    message => 'Type check failed for return value; expected Int:D but got Int (Int)',
    'a routine return check reads the same';
