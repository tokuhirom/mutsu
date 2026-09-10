use Test;

plan 6;

# A binding type-check failure on a parameter whose expected type cannot be
# ruled out statically (e.g. Sub vs Block) is a runtime
# X::TypeCheck::Binding::Parameter (which ISA X::TypeCheck::Binding) — NOT a
# compile-time X::TypeCheck::Argument. The presence of a *different*, simply-typed
# parameter (Str) in the same signature must not reclassify the failure.

sub takes-sub (Sub $code, Str $a, Str $b) { $a.WHAT }

throws-like { takes-sub(-> { die "x" }, "a", "b") },
    X::TypeCheck::Binding,
    'pointy block bound to Sub param throws X::TypeCheck::Binding';

lives-ok { takes-sub(sub { die "x" }, "a", "b") },
    'anonymous sub satisfies Sub param';

# With an optional trailing param the classification is unchanged.
sub takes-sub2 (Sub $code, Str $a, Str $b?) { $a.WHAT }

throws-like { takes-sub2(-> { die "x" }, "a") },
    X::TypeCheck::Binding,
    'pointy block bound to Sub param (optional last) throws X::TypeCheck::Binding';

# A simply-typed parameter bound to a literal of the wrong type is the
# compile-time-detectable case and stays X::TypeCheck::Argument.
throws-like 'sub f(Int $x) { }; f("hi")',
    X::TypeCheck::Argument,
    'Int param bound to Str literal throws X::TypeCheck::Argument';

throws-like 'sub f(Str) { }; f(42)',
    X::TypeCheck::Argument,
    'type-only Str param bound to Int literal throws X::TypeCheck::Argument';

# The runtime Sub-vs-Block failure also reports the right concrete subtype.
my $ex;
{
    takes-sub(-> { 1 }, "a", "b");
    CATCH { default { $ex = $_ } }
}
isa-ok $ex, X::TypeCheck::Binding::Parameter,
    'concrete exception is X::TypeCheck::Binding::Parameter';
