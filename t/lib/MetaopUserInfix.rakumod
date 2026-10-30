# Re-exports the operators through an `EXPORT` hook, as Red does, so the parser
# sees the `use` but the operators are not declared in the importing file.
use MetaopUserInfix::Ops;

sub exports(@experimental --> Map()) {
    MetaopUserInfix::Ops::EXPORT::ALL::,
    '&table' => sub (Str $name) { MetaopUserInfix::Ops::Table.new(:$name) },
}

multi EXPORT(+@experimental) { exports(@experimental) }
