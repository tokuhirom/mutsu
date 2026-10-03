use Test;

plan 6;

# Rakudo leaves `.signature` of the system objects unset: the `Blob` type
# object, so interpolating it yields "" (with a warning) instead of dying the
# way a defined Blob's stringification does.
for $*KERNEL, $*DISTRO, $*VM, $*RAKU, $*RAKU.compiler -> $o {
    ok $o.signature === Blob, "{$o.^name}.signature is the Blob type object";
}
{
    CONTROL { when CX::Warn { .resume } }
    is "x{$*VM.signature}y", 'xy', 'it interpolates as the empty string';
}
