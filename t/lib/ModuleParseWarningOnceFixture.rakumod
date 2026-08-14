unit module ModuleParseWarningOnceFixture;

# Deliberately duplicates the `is export` trait to trigger the parser's
# "Duplicate 'is export' trait" warning -- see t/module-parse-warning-once.t.
sub module-parse-warning-once-hello() is export is export { "hi" }
