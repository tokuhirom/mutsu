use Test;

plan 19;

# An omitted optional parameter (no default) binds its implicit default: the
# parameter's NOMINAL type object, which for a subset is the refinee (`Str`
# for `subset S of Str`, `Int` for `UInt`) -- not the subset itself. Rakudo
# then checks the subset's predicate against that default, so a predicate that
# rejects undefined values makes the bare call die (#10954).

subset S of Str where { .defined && $_ eq "x" };
subset T of Str where { !.defined || $_ eq "x" };

my $note = "The parameter is optional and was not passed an argument, so its\n"
    ~ "implicit default value was checked against the constraint. Give the\n"
    ~ "parameter a default value that satisfies the constraint, mark it as\n"
    ~ "required, or make the constraint accept the implicit default.";

sub named-s(S :$s) { "called" }
throws-like { named-s() }, X::TypeCheck::Binding::Parameter,
    message => "Constraint type check failed in binding to parameter '\$s'; expected S but got Str (Str)\n$note",
    'omitted optional named subset param checks its implicit default';
is named-s(:s<x>), "called", 'a passed value that satisfies the subset still binds';

sub positional-s(S $s?) { "called" }
throws-like { positional-s() }, X::TypeCheck::Binding::Parameter,
    'omitted optional positional subset param checks its implicit default';

class C {
    method named(S :$s) { "called" }
    method positional(S $s?) { "called" }
}
throws-like { C.named }, X::TypeCheck::Binding::Parameter,
    'omitted optional named subset param of a method';
throws-like { C.positional }, X::TypeCheck::Binding::Parameter,
    'omitted optional positional subset param of a method';
is C.positional("x"), "called", 'the method still binds a passed value';

multi m(S $s?) { "subset" }
multi m(Int $i) { "int" }
throws-like { m() }, X::Multi::NoMatch,
    'a multi candidate is not selected when its omitted subset param fails';

# A predicate that accepts undefined values lets the call through, and the
# parameter holds the refinee's type object.
sub named-t(T :$t) { $t.WHAT }
is named-t().^name, 'Str', 'omitted named subset param binds the refinee type object';
sub positional-t(T $t?) { $t.WHAT }
is positional-t().^name, 'Str', 'omitted positional subset param binds the refinee type object';
sub with-uint(UInt :$u) { $u.WHAT }
is with-uint().^name, 'Int', 'omitted UInt param binds Int';

subset I of Int;
subset J of I;
sub nested(J :$j) { $j.WHAT }
is nested().^name, 'Int', 'a subset of a subset binds the innermost refinee';

# A `my` variable of a subset type keeps the subset type object before 6.e.
my T $v;
is $v.WHAT.^name, 'T', 'a subset-typed variable still holds the subset type object';

# The predicate runs once for a plain sub.
my $count = 0;
subset Counted of Int where { $count++; True };
sub counted(Counted :$c) { }
counted();
is $count, 1, 'the subset predicate runs once for an omitted param';

# A default is bound instead, and is what the subset checks.
sub defaulted(S :$s = "x") { $s }
is defaulted(), "x", 'a defaulted subset param binds its default';

# An explicit `where` on an omitted optional also explains the implicit default.
sub where-pos(Int $x? where { False }) { }
throws-like { where-pos() }, X::TypeCheck::Binding::Parameter,
    message => "Constraint type check failed in binding to parameter '\$x'; expected anonymous constraint to be met but got Int (Int)\n$note",
    'omitted positional `where` failure carries the implicit-default note';
sub where-named(Int :$x where { False }) { }
throws-like { where-named() }, X::TypeCheck::Binding::Parameter,
    message => "Constraint type check failed in binding to parameter '\$x'; expected anonymous constraint to be met but got Int (Int)\n$note",
    'omitted named `where` failure carries the implicit-default note';
sub where-passed(Int :$x where { False }) { }
throws-like { where-passed(:x(3)) }, X::TypeCheck::Binding::Parameter,
    message => "Constraint type check failed in binding to parameter '\$x'; expected anonymous constraint to be met but got Int (3)",
    'a passed argument that fails `where` gets no implicit-default note';

# A subset failure on a passed value spells the value as `.raku` does.
sub passed-s(S $s) { }
throws-like { passed-s("a") }, X::TypeCheck::Binding::Parameter,
    message => "Constraint type check failed in binding to parameter '\$s'; expected S but got Str (\"a\")",
    'a passed Str rejected by a subset is spelled with quotes';
throws-like { passed-s(Str) }, X::TypeCheck::Binding::Parameter,
    message => "Constraint type check failed in binding to parameter '\$s'; expected S but got Str (Str)",
    'a passed type object rejected by a subset is spelled by name';
