# Fixture for t/modules/import-export/require-in-block-binds-lexically.t:
# one exported routine, variable, class and constant for `require` to list.
unit module RequireScopedImports;
sub rsi-f is export { "rsi-f" }
our $rsi-v is export = "rsi-v";
class RsiC is export { method hi { "rsi-c" } }
our constant RSI-K is export = 42;
