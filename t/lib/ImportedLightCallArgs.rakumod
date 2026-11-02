unit module ImportedLightCallArgs;
sub ilc-inc($x) is export { $x + 1 }
sub ilc-id($x) is export { $x }
sub ilc-named($x, :$k = 'none') is export { "$x/$k" }
sub ilc-slurpy(*@a) is export { @a.elems }
