unit module EvalEnumKeyPrivateFixture;

# An enum declared inside a package is visible to that package's own code and
# not to the importer, so an EVAL run by the importer must not resolve its key.
enum Priv (:EvalFixturePK(7));

sub peek-key is export { EvalFixturePK }
