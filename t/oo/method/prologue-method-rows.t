use Test;

# ADR-11276 §9.111: the receiver-specific arms of `methods_0arg`'s prologue
# are rows, and the receivers they answered reach them.

plan 10;

# `Unicode` is a type object only (`DispatchShape::Unicode`).
isa-ok Unicode.version, Version, 'Unicode.version';
ok Unicode.version >= v15, 'Unicode.version names the shipped tables';
is Unicode.NFG, True, 'Unicode.NFG';
ok Unicode.^can('version'), 'Unicode.^can(version)';

# `List.from` and `List.to` (Rakudo declares them on `List`).
my @m = "abcd" ~~ m:g/<[bc]>/;
is @m.from, 1, 'a list of matches: .from is the first one\'s';
is @m.to, 3, 'a list of matches: .to is the last one\'s';
is [].to, Nil, 'an empty list: .to is Nil';
is ().from, Nil, 'an empty list: .from is Nil';
throws-like { (1, 2).to }, X::Method::NotFound, 'an element with no .to throws';
throws-like { @m.ast }, X::Method::NotFound, 'List has no .ast';
