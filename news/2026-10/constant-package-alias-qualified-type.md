# Types and enum values resolve through a constant package alias

`constant E = Outer::Inner; E::Status::Started` used to give a bare package
named `E::Status::Started` instead of the enum value, and `E::Nested` gave
`E::Nested` instead of `Outer::Inner::Nested`. A constant bound to a package
was already accepted as a qualifier for a call (`E::f()`) and for `&E::f`. The
bareword path, which resolves type names and enum values, now uses the same
`resolve_package_alias_prefix` rewrite.

Found via the `Terminal::MultiProgress` distribution, whose `t/02-event`
compares against `Event::Status::Started` through
`constant Event = Terminal::MultiProgress::Event`. That file now passes. Its
`t/04`, `t/05` and `t/06-odometer` capture output by wrapping
`$*OUT.^find_method('print')`, which mutsu does not honour yet (#11314).
`$E::v` through such an alias is #11315.
