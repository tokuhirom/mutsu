use v6;
use Test;
use lib 't/lib';

# From HTML::Tag (`HTML::Tag::{$name}.new`): a module that declares
# `class Ns::x` itself, without declaring `Ns::<its own name>`, still
# contributes those classes to the `Ns::` stash. Only its dependencies' types
# are transitive (roast S10-packages/precompilation.t keeps those hidden).

plan 3;

use StashOwnBase::Tags;

is-deeply StashOwnBase::.keys.sort.List, <h1 p>, 'own classes are stash keys';
my $name = 'h1';
is StashOwnBase::{$name}.^name, 'StashOwnBase::h1', 'runtime-key stash lookup finds it';
is StashOwnBase::{'p'}.new.^name, 'StashOwnBase::p', 'and it is constructible';

# vim: expandtab shiftwidth=4
