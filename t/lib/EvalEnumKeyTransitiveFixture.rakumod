use EvalEnumKeyGlobalFixture;

# Loads a package-less module that declares an enum, and uses its key itself.
# What a module `use`s is merged into the module, not into whoever loads it.
sub via-key is export { EvalFixtureSA }
