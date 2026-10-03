# Fixture for t/lang/code-var-free-read-ignores-caller-shadow.t: a module's
# file-scope `my &enc`, read by its own routines.
unit module CodeVarFreeReadUnit;

my &enc = sub ($m) { "unit" };

our sub unit-value() is export { my $e = &enc; $e("q") }
our sub unit-call() is export { enc("q") }
