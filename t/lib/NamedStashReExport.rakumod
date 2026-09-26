unit module NamedStashReExport;

# The re-export idiom `Sparrow6::DSL` uses: import a helper module's subs, then
# copy each one into this module's own `EXPORT::DEFAULT` stash by writing to
# the stash from outside it, under a key computed at BEGIN time.
use NamedStashReExportInner;

sub own () { 'own' }

my package EXPORT::DEFAULT { }

BEGIN for <&tags &shout> {
    EXPORT::DEFAULT::{$_} = ::($_)
}

BEGIN EXPORT::DEFAULT::<&own> := &own;
