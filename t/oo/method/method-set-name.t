use v6;
use Test;

# #12021: `Method.set_name` renames the method's code object. The rename
# belongs to the class's method table entry, so it shows on every later read
# of that table, while dispatch still goes by the declared name.

plan 7;

class C { method m { 1 } }
my $m = C.^find_method("m");
is $m.^name, 'Method', 'find_method answers a Method';
$m.set_name("mm");
is $m.name, 'mm', 'set_name renames the object it was called on';
is C.^find_method("m").name, 'mm', '... and a later find_method read';
is C.^lookup("m").name, 'mm', '... and a later lookup read';
is C.^methods.first(*.name eq 'mm').defined, True, '... and ^methods';
is C.new.m, 1, 'the method is still dispatched by its declared name';
is $m.set_name("zz"), 'zz', 'set_name answers the new name';
