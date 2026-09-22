unit module CodeFilePackageSuffixFixture;

# Fixture for t/modules/code-file-module-package-suffix.t: routines declared
# at several nesting depths inside one real compilation unit, so the test can
# check that `.file` reports THIS module's own package identity for all of
# them -- not each routine's own (possibly deeper) lexical package.

sub fixture-sub() is export { 1 }

class FixtureClass is export {
    method fixture-method() { 1 }
}

grammar FixtureGrammar is export {
    token fixture-token { \d+ }
}
