use Test;

# A `my sub` declared inside a routine re-executes its RegisterSub opcode on
# every call to the enclosing routine. The registrar recognizes the structurally
# identical re-registration as an idempotent no-op (compile-time fingerprint),
# which must not change the observable behaviour: each invocation still sees a
# fresh lexical scope, captures the right outer values, and the inner sub stays
# lexically scoped.

plan 12;

# Captured outer value is per-call, not frozen at first registration.
sub outer($n) {
    my sub helper() { $n * 2 }
    helper();
}
is outer(5), 10, 'nested sub captures per-call $n (first call)';
is outer(7), 14, 'nested sub captures per-call $n (second call)';
is outer(5), 10, 're-registration is idempotent, captures still correct';

# Loop body: each iteration's nested sub captures that iteration's value.
my @r;
for 1..3 -> $i {
    my sub h() { $i }
    @r.push(h());
}
is @r.join(','), '1,2,3', 'loop-local nested sub captures per-iteration value';

# Recursion through a nested sub across repeated enclosing calls.
sub fact-outer($n) {
    my sub fact($x) { $x <= 1 ?? 1 !! $x * fact($x - 1) }
    fact($n);
}
is fact-outer(5), 120, 'recursive nested sub works (first call)';
is fact-outer(6), 720, 'recursive nested sub works (repeated call)';

# Mutual recursion relies on the hoisted registration being visible before the
# textual position of the second sub.
sub mutual($n) {
    my sub is-even($x) { $x == 0 ?? True  !! is-odd($x - 1) }
    my sub is-odd($x)  { $x == 0 ?? False !! is-even($x - 1) }
    is-even($n);
}
ok mutual(4),  'mutually recursive nested subs (even)';
nok mutual(7), 'mutually recursive nested subs (odd)';

# The inner sub must remain lexically scoped — not leak to the outer scope.
sub scoped() {
    my sub secret() { 42 }
    secret();
}
is scoped(), 42, 'nested sub is callable inside its scope';
my $leaked = try { secret() };
nok $leaked.defined, 'nested sub does not leak out of its scope';

# A re-registered nested sub returning a closure: the returned closures from
# different calls capture different scopes.
sub make-adder($base) {
    my sub adder($x) { $base + $x }
    &adder;
}
my $add10 = make-adder(10);
my $add100 = make-adder(100);
is $add10(5), 15, 'closure from first registration keeps its scope';
is $add100(5), 105, 'closure from later registration has its own scope';
