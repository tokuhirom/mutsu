use Test;

# A method object of a core type with no builtin row (`Match.pos`, `.from`,
# `.orig`) called with an instance dispatches on it (Data::Dump's Match dump:
# `$obj.^can($p)[0].($obj)`).

plan 5;

my $m = "abcd" ~~ /b(c)/;
is $m.^can('pos')[0].($m), 3, 'pos';
is $m.^can('from')[0].($m), 1, 'from';
is $m.^can('orig')[0].($m), 'abcd', 'orig';
my $s = "abc";
is $s.^can('uc')[0].($s), 'ABC', 'a method with a row still works';
is $s.^can('chars')[0]($s), 3, 'called without the dot';
