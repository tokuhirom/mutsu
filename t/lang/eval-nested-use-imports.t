use Test;
use lib 't/lib';

# What a module imports is in scope for an EVAL'd unit's CHECK-time validation
# wherever the `use` sits, and for an EVAL called from a scope that imported it
# (#13441).
plan 12;

is EVAL(q[{ use EvalNestedUseTagged; plain-fn }]), 1, 'a routine imported in a nested block';
is EVAL(q[if True { use EvalNestedUseTagged; plain-fn }]), 1, 'in a branch body';
throws-like { EVAL q[{ use EvalNestedUseTagged; tagged-fn }] }, X::Undeclared::Symbols,
    'a tagged export is not in the default import';
is EVAL(q[{ use EvalNestedUseTagged :T; tagged-fn }]), 2, 'a tagged import brings the tagged routine';
is EVAL(q[{ use EvalNestedUseTagged :M; both-fn }]), 3, 'a :DEFAULT and :M export under :M';
is EVAL(q[{ use EvalNestedUseTagged; both-fn }]), 3, 'and by default';
is EVAL(q[{ use EvalNestedUseTagged; bright }]).raku, 'Shade::bright', 'an enum value';
throws-like { EVAL q[{ use EvalNestedUseTagged; no-such-fn }] }, X::Undeclared::Symbols,
    'an unknown name next to the import is undeclared';

{
    use EvalNestedUseTagged;
    is EVAL(q[plain-fn]), 1, 'an EVAL sees a routine its caller imported';
    sub caller-eval { EVAL q[plain-fn] }
    is caller-eval(), 1, 'also from a routine in that scope';
    throws-like { EVAL q[no-such-fn] }, X::Undeclared::Symbols, 'an unknown name still is undeclared';
}
throws-like { EVAL q[plain-fn] }, X::Undeclared::Symbols, 'the import does not outlive its block';
