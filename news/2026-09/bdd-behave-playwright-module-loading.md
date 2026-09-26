# BDD::Behave::Playwright module loading

mutsu now loads BDD::Behave::Playwright 0.9.1 with its dependency closure. This covers typed class-level aggregate attributes declared with `of`, and lazy unit-module proto exports that coexist with imported bare-file wrappers. The distribution's non-browser tests now match Rakudo; browser tests remain gated by their missing Playwright sidecar dependencies.
