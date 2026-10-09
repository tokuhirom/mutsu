# Multi dispatch: a subset refinement no longer outranks an unrelated role parameter

A candidate whose parameter was a `subset` could be ranked as incomparable with, and
declared before, a candidate taking an unrelated role at the same position, so the
declaration-order fallback picked the wider one (XML::Class's `deserialise` multis then
recursed forever). Parameters whose nominal types are unrelated are now tied, as in Rakudo.

Also from the Audio::Hydrogen distribution (all four test files now pass): an attribute
trait whose argument names a `sub` declared later in a class that `does` a role
(`is xml-serialise(&from-version)`) no longer reads as an unknown trait during the
role-composition stand-in pass.
