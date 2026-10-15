use Test;

# ADR-11276 §9.111: the receiver-specific arms of `methods_0arg`'s prologue
# are rows, and the receivers they answered reach them.

plan 4;

# `Unicode` is a type object only (`DispatchShape::Unicode`).
isa-ok Unicode.version, Version, 'Unicode.version';
ok Unicode.version >= v15, 'Unicode.version names the shipped tables';
is Unicode.NFG, True, 'Unicode.NFG';
ok Unicode.^can('version'), 'Unicode.^can(version)';
