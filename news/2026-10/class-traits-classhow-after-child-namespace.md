# A class's method traits see ClassHOW after a child module vivified its namespace

When `Foo::Util` was loaded before `Foo.rakumod`'s `unit class Foo`, the namespace `Foo`
was recorded as a bare package until the class declaration finished, so a method
trait's `$*PACKAGE.HOW` was a `PackageHOW`. `Method::Also`'s `is also<Str gist>` then
never composed its aliases (CSS::Properties `t/01-property-basic.t` 21 of 21, was 20).
The vivified package record is now dropped when the class declaration starts.
Pinned in `t/oo/trait/class-traits-see-classhow-after-child-namespace.t`.
