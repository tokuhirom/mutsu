# A package-less module whose file-scope constants are published bare.
constant TRIM-BEFORE = "\x[E000]";
constant TRIM-AFTER  = "\x[E001]";
class PackagelessTrimUser is export { method t { TRIM-BEFORE.ord } }
