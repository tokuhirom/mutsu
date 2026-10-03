use Test;
use lib 't/lib';

# A bare `is marked` on a class inside `unit module M` dispatches
# `trait_mod:<is>(M::Plain, :marked)`, once -- not `:M::marked`, which no
# candidate accepts and which then failed as an unknown parent (#11349).

plan 4;

use UnitModuleBareIsTrait;

my @seen = @UnitModuleBareIsTrait::seen;
is @seen.grep('marked UnitModuleBareIsTrait::Plain').elems, 1,
    'a bare trait on a class runs once, under its written name';
is @seen.grep('marked UnitModuleBareIsTrait::WithArg').elems, 1,
    'the argument form still runs once';
is @seen.grep('marked UnitModuleBareIsTrait::Roled').elems, 1,
    'a bare trait on a role runs once';
ok UnitModuleBareIsTrait::Plain.^parents.map(*.^name).none eq 'UnitModuleBareIsTrait::marked',
    'the trait name is not a parent';
