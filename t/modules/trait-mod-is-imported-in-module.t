use v6;
use lib 't/lib';

# From Pop (Pop::Config is Singleton) and Qt::QtWidgets (class QSize is QtObject):
# a class declared inside a module that itself `use`s the provider of a
# `trait_mod:<is>` is subject to that trait. The main program imports nothing
# that defines a trait_mod:<is> -- not even Test, whose own one would make the
# deferral to custom traits unconditional -- so this file prints TAP by hand.

say '1..3';

use TraitModIsConsumer;

say (TraitModIsConsumer.new.hi eq 'hi from the singleton' ?? 'ok' !! 'not ok'),
    ' 1 - the module-level class loaded';
say (TraitModIsConsumer.new === TraitModIsConsumer.new ?? 'ok' !! 'not ok'),
    ' 2 - and the trait (a singleton role) was applied';

my $unknown = try { EVAL 'class NoSuchTraitNoImport is NotAnyTraitAtAll { }'; 'lived' };
say ($!.defined && $!.^name eq 'X::Inheritance::UnknownParent' ?? 'ok' !! 'not ok'),
    ' 3 - an unknown name nobody provides is still an unknown parent';
