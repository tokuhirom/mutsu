use Test;

# A plain `$` parameter of a method holds a List argument as one item, as a
# sub's does: `%h{$k}` stores one key, not a slice (found with #12903).

plan 4;

class C {
    method key($k) { my %h; %h{$k} = 1; %h.elems }
    method repr($k) { $k.raku }
    method typed(List $k) { my %h; %h{$k} = 1; %h.elems }
}
sub key($k) { my %h; %h{$k} = 1; %h.elems }

is C.key((1, 2)), 1, 'a List argument is one hash key';
is C.repr((1, 2)), '$(1, 2)', 'the parameter holds the List itemized';
is C.typed((1, 2)), 1, 'a typed parameter too';
is key((1, 2)), 1, 'the same as a sub';
