use EqvHookRole;
class EqvHookAst is export { }
class EqvHookImpl does EqvHookRole is export { }

# The operator candidates live in a module of their own and reach the importer
# through a `sub EXPORT` that re-exports that module's `EXPORT::ALL::` stash.
module EqvHookInner {
    multi infix:<eqv>(EqvHookAst $a, EqvHookAst $b) is export { "OPS" }
    multi infix:<eqv>(EqvHookAst $a, $b) is export { "OPS2" }
}

sub EXPORT { %( EqvHookInner::EXPORT::ALL::, 'EqvHookAst' => EqvHookAst, 'EqvHookImpl' => EqvHookImpl ) }
