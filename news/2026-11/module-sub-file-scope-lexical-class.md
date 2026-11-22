# A module routine's own `my class` is not the importer's same-named class

In a `unit module M` file, `my class R { }; sub make-r is export { R.new }`
built the importing program's `class R` whenever the program declared one;
rakudo builds `M::R`. A module body runs against its importer's env, and at
the end of the load the body's short binding of its lexical type is dropped.
The module routine then resolved `R` through the env it runs against, which
the program's class answered.

The load now records each of the unit's own file-scope lexical types in the
unit's bucket under its type-only key. A bare name read by the module's
routines resolves there before the importer's binding does (#18677).
