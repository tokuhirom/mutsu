use Test;

# `IO::Path`'s `IO::Path:D:` methods refuse a type object through their rows
# (ADR-11276, `RowFlags::DEFINITE`), whatever the call's arity, and so do the
# per-OS subclasses' type objects; the `multi` ones find no candidate.

plan 12;

for <e f d s basename dirname lines words open> -> $m {
    throws-like { IO::Path."$m"() }, X::Parameter::InvalidConcreteness,
        "IO::Path.$m refuses the type object";
}
throws-like { IO::Path.split }, X::Parameter::InvalidConcreteness,
    'a row of another arity refuses it too';
throws-like { IO::Path::Unix.e }, X::Parameter::InvalidConcreteness,
    'IO::Path::Unix.e';
throws-like { IO::Path.extension }, X::Multi::NoMatch, 'IO::Path.extension';
