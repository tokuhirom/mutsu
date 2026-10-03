use v6;
use lib 't/lib';
use Test;

# `enum A::Level is export` exports the short name `Level` too, so a module
# that `use`s it can write `Level::error` both at its top level and inside
# its routines -- even when the importer only takes a tagged export
# (`:configure`) and so never sees `Level` itself (LogP6 has this shape).

use QualEnumLevel :configure;

plan 5;

is +$trace, 1, 'Level::trace at module top level';
is +level-error(), 5, 'Level::error inside a module routine';
is level-error().key, 'error', 'it is the enum value';
is level-name(2), 'debug', 'Level(...) coercion inside a module routine';
is $debug-name, 'debug', 'Level(...) coercion at module top level';
