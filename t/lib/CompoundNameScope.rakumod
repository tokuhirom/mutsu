unit module CompoundNameScope;

our sub helper() is export { 'module helper' }

# `NL::Searcher`-shaped nesting: the class IS lexically inside the module, so a
# bare routine name in its body must find the module's routine.
class Searcher is export {
    method call-helper() { helper() }
}
