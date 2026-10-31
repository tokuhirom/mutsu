# `class X is Trait` still finds a module-imported `trait_mod:<is>` when other candidates are visible

Follow-up to the module-import fix (#15835): `use Pop::Graphics` still died with `'Pop::Config'
cannot inherit from 'Singleton'` because Graphics has a `use NativeCall` inside a method body.
That brings NativeCall's own `trait_mod:<is>` candidates into view, so the by-name dispatch of
`is Singleton` found candidates, none matched, and the class was an unknown parent without the
provider's candidate (imported into the module's compunit) ever being tried. A no-candidate answer
now also tries the module-scope `&trait_mod:<is>`; only when that does not match either is the
name an unknown parent.
