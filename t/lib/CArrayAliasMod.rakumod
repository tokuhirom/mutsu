unit module CArrayAliasMod;

# The shape of upstream NativeCall: a namespaced class exported under its short
# name through a `my constant` alias.
our class Types::CArray { }

my constant CArray is export = CArrayAliasMod::Types::CArray;
