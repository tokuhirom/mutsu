# `class X is Trait` inside a module reaches a `trait_mod:<is>` the module imported

`class Pop::Config is Singleton` (Pop) and `class QSize is QtObject` (Qt::QtWidgets) sit in
modules that `use` the provider of a `multi sub trait_mod:<is>(Mu:U $doee, :$Singleton!) is
export`. mutsu answered `'X' cannot inherit from 'Singleton' because it is unknown`: the
by-name search for `trait_mod:<is>` does not see a candidate imported into the *module's* own
compunit (only into the main program). Both the unknown-parent deferral and the trait dispatch
now also look for any registered `trait_mod:<is>` and fall back to the module-scope `&trait_mod:<is>`
code object; a name nobody provides is still `X::Inheritance::UnknownParent`.
