use Test;
use lib 't/lib';

# A sub the scope declares itself wins over an import of the same name, in a
# block as at the top level (Rakudo reports a redeclaration; mutsu has always
# let the local sub win), for a bare call.

plan 2;

{
    use Issue8214ImportShadow <shadowed>;
    sub shadowed() { "local" }
    is shadowed(), "local", 'a local sub shadows a sub EXPORT import in a block';
}

{
    use Issue8214ImportShadow <shadowed>;
    is shadowed(), "imported", 'a sibling block without the sub keeps the import';
}
