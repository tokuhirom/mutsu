use Test;

plan 6;

class Bar {}
class Pt { has $.x = 1; has $!y = 2; }

sub msg(&c) { try c(); $!.message }

# X::Assignment::RO names the object by its `.gist`, as rakudo does.
is msg({ my %h is Bar = a => 1 }), 'Cannot modify an immutable Bar (Bar.new)', 'tying a hash to a plain class and initialising it';
is msg({ Bar.new = 3 }), 'Cannot modify an immutable Bar (Bar.new)', 'assigning to a freshly built object';
is msg({ Pt.new = 3 }), 'Cannot modify an immutable Pt (Pt.new(x => 1))', 'public attributes show in the repr';
is msg({ my \c = 5; c = 6 }), 'Cannot modify an immutable Int (5)', 'a builtin value is unchanged';

try { Bar.new = 3 };
isa-ok $!, X::Assignment::RO, 'the exception type';
is $!.typename, 'Bar', 'the typename attribute names the class';
