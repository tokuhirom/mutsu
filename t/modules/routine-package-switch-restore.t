use Test;

# A routine's body runs under its DECLARING package, and the caller's package
# is put back on return. Both halves of that are side-channel bookkeeping on
# `Interpreter::current_package` (plus its interned `current_package_sym`
# mirror), performed by three separate mechanisms that have to agree:
#
#   - `enter_routine_package` / `leave_routine_package`
#     (src/vm/vm_call_eligibility.rs) for the light and fast call paths,
#   - `CurrentPackageGuard` via `enter_package_guarded_sym`
#     (src/runtime/accessors_stack.rs) for the general named binder
#     (`call_compiled_function_named_inner`) and for closure dispatch,
#   - `exec_package_scope_op` for a `package`/`module` block body.
#
# #8686 Phase 1 made all three carry the saved package as an interned `Symbol`
# instead of an owned `String`, and made both the switch and its restore skip
# their writes when the package has not actually moved -- the dominant hot
# shape being a routine calling a sibling declared in its own package, where
# each write only put back the value that was already there. That rests on an
# invariant (#7576): `current_package` and `current_package_sym` never
# disagree, so the string is recoverable from the symbol and need not be saved.
#
# What this file pins is the OBSERVABLE consequence, not the saving: if a
# switch were skipped when it should not be, or a restore rebuilt the wrong
# text, an unqualified name inside a routine would resolve against the wrong
# package -- silently answering with another package's sub or `our` variable
# rather than failing. Each assertion below therefore reads a name whose
# meaning depends on which package is current at that moment.
#
# All 12 assertions pass under rakudo.

plan 12;

our $tag = 'GLOBAL';
sub who() { 'GLOBAL-who' }

module P {
    our $tag = 'P';
    our sub who() { 'P-who' }
    # Light-call shape: plain scalar params, no traits, no native types, so
    # this reaches the `enter_routine_package` path.
    our sub light() { $tag }
    # General-binder shape: a `where` constraint disqualifies the light paths,
    # so this reaches the `CurrentPackageGuard` path instead.
    our sub heavy($n where * >= 0) { "$tag/$n" }
    our sub probe() { who() }
    our sub calls-q() { Q::light() }
    our sub after-foreign() { Q::probe(); who() }
}

module Q {
    our $tag = 'Q';
    our sub who() { 'Q-who' }
    our sub light() { $tag }
    our sub probe() { who() }
    our sub calls-p() { P::light() }
}

is P::light(), 'P', 'a light-path call runs under its declaring package';
is Q::light(), 'Q', 'and so does a call into another package';
is $tag, 'GLOBAL', 'the caller package variable is unchanged after a package call';
is who(), 'GLOBAL-who', 'the caller package is restored, so a bare name still resolves here';
is P::heavy(1), 'P/1', 'a general-binder call runs under its declaring package';
is P::probe(), 'P-who', 'an unqualified call inside a package sub stays in that package';
is Q::probe(), 'Q-who', 'and resolves to the other package from there';
is P::after-foreign(), 'P-who', 'the declaring package is restored after a foreign call';
is Q::calls-p(), 'P', 'a cross-package nested call enters the callee package';

# Repeat calls take the memoized/cached dispatch entries, which is where a
# skipped switch would show up: the first call would be right and the rest
# wrong, or vice versa.
my @seen;
for ^5 { @seen.push(P::light()); @seen.push(Q::light()); @seen.push(P::probe()); }
is @seen.unique.sort.join(','), 'P,P-who,Q', 'repeat calls keep each package distinct';
is who(), 'GLOBAL-who', 'GLOBAL resolution survives a loop of package calls';
is P::probe(), 'P-who', 'and package resolution still works after the loop';
