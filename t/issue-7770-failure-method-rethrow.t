use v6;
use Test;

# An unhandled Failure must throw when a method call tries to use its value.
# Handling/introspection accessors remain usable and mark the Failure handled
# according to Raku's semantics.
plan 9;

my $caught-type;
{
    my $failure = Failure.new;
    $failure.lines.elems;
    CATCH { default { $caught-type = .^name } }
}
is $caught-type, 'X::AdHoc',
    'an ordinary method call on an unhandled Failure rethrows its exception';

my $open-caught-type;
{
    my $failure = 'mutsu-issue-7770-no-such-file-9f2a'.IO.open;
    $failure.lines.elems;
    CATCH { default { $open-caught-type = .^name } }
}
is $open-caught-type, 'X::AdHoc',
    'a failed IO::Path.open followed by .lines rethrows the carried exception';

my $accessor-failure = Failure.new;
is $accessor-failure.defined, False, 'Failure.defined remains a handling accessor';
is $accessor-failure.Bool, False, 'Failure.Bool remains a handling accessor';
is $accessor-failure.so, False, 'Failure.so remains a handling accessor';
is $accessor-failure.^name, 'Failure', 'Failure.^name remains usable';
is $accessor-failure.exception.^name, 'X::AdHoc',
    'Failure.exception remains usable';
is $accessor-failure.DEFINITE, True, 'Failure.DEFINITE remains usable';

my $with-branch = '';
my $with-failure = Failure.new;
with $with-failure {
    $with-branch = 'then';
} else {
    $with-branch = 'else';
}
is $with-branch, 'else', 'with/without can inspect an unhandled Failure';

done-testing;
