use Test;

# Signature.gist and Signature.raku are rows of the method table (ADR-11276 §9.39).

plan 6;

sub f(Int $x, Str :$y) { }
is &f.signature.gist, '(Int $x, Str :$y)', 'Signature.gist';
is &f.signature.raku, ':(Int $x, Str :$y)', 'Signature.raku';
is :(Int $a, $b?).gist, '(Int $a, $b?)', 'a literal signature, gist';
is :(Int $a, $b?).raku, ':(Int $a, $b?)', 'a literal signature, raku';
is :().gist, '()', 'an empty signature, gist';
is :().raku, ':()', 'an empty signature, raku';

# vim: expandtab shiftwidth=4
