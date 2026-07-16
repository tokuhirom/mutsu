use Test;

# A coercion-typed parameter (e.g. `Str() $x`) must dispatch the target
# coercion method even when it is a *native* builtin method rather than a
# user-declared one. IO::Path is the canonical case: `IO::Path.Str` is a
# native method, so `sub f(Str() $uri) {...}; f($some-io-path)` used to fail
# with "No such method 'Str' for invocant of type 'IO::Path'" because the
# coercion path routed native methods through the user-method runner.
# This is exactly what zef's `Zef::Service::Shell::tar.extract-matcher(Str() $uri)`
# hits when passed a `$candi.uri` IO::Path during the install pipeline's
# extract phase.

plan 8;

sub take-str(Str() $x --> Bool:D) { return so $x.lc.ends-with('.gz') }
ok  take-str('/tmp/x.tar.gz'.IO), 'Str() param coerces IO::Path via native .Str (matches)';
nok take-str('/tmp/x.tar'.IO),    'Str() param coerces IO::Path via native .Str (no match)';

sub name-of(Str() $x) { $x.^name }
is name-of('/a/b'.IO), 'Str', 'Str() coercion of IO::Path yields a Str';
is name-of(42),        'Str', 'Str() coercion of Int still works';

# User-declared coercion methods must keep working (regression guard for the
# fix, which now only routes *user* methods through run_instance_method).
class WithStr is Cool {
    has $.a = 'coerced';
    method Str() { $.a }
}
is name-of(WithStr.new), 'Str', 'Str() coercion still runs a user .Str method';
ok take-str('archive.GZ'), 'plain Str arg to Str() param, .lc applied';

# Int() coercion of a native type dispatches the native .Int method.
sub int-of(Int() $x) { $x.^name }
is int-of('42'), 'Int', 'Int() coercion of Str still works';

# IO::Path.Int is a native method too; raku returns a Failure for a
# non-numeric path, so a numeric-looking basename coerces cleanly.
sub abs-int(Int() $x) { $x }
is abs-int(7), 7, 'Int() coercion passes through an Int';
