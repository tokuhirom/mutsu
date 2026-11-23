# The bundled Template::Mustache is re-vendored at 1.2.7

`modules/Template-Mustache/` now carries upstream
`Template::Mustache:ver<1.2.7>:auth<zef:raku-community-modules>` (tag `1.2.7`,
commit `642eafe6`), copied verbatim from the fez release, up from 1.2.6.

The library code only bumps its own version; the release moves the test
dependency to `JSON::Fast:ver<0.21+>:auth<zef:timo>`, which the upstream
suite's `t/lib/Template/Mustache/TestUtil.rakumod` `use`s directly. That is why
this re-vendor lands after the bundled `JSON::Fast` reached 0.21.0: against the
older bundled copy, every spec-driven test file would fail to load its helper.
`batteries.lock` pins the new commit.
