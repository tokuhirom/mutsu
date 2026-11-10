use Test;

# From the Red distribution (Red::Compiler::Template.path):
#   my $locals = try %*RED-COMPILER-LOCALS;
#   $locals.DEFINITE && ...
# A `try` whose value is a Failure (here the lazy Failure of an undeclared
# dynamic variable) yields an undefined Any, not a Failure.

plan 8;

sub probe() {
    my $locals = try %*NOT-DECLARED-ANYWHERE;
    $locals.DEFINITE
}
nok probe(), "try %*undeclared: the result is not DEFINITE";

my $s = try $*NOT-DECLARED-ANYWHERE;
is $s.WHAT.^name, "Any", "try \$*undeclared is Any";
my $a = try @*NOT-DECLARED-ANYWHERE;
is $a.WHAT.^name, "Any", "try @*undeclared is Any";
my $b = try { $*NOT-DECLARED-ANYWHERE };
is $b.WHAT.^name, "Any", "try block yielding the lookup Failure is Any";

my $x = try { Failure.new("a") };
is $x.WHAT.^name, "Any", 'try { Failure.new } is Any';
is $!.^name, "X::AdHoc", '$! holds the exception';

my $ok = try { 42 };
is $ok, 42, "a normal value passes through";
ok !$!.defined, '$! is cleared after a successful try';
