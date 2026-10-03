use Test;

plan 8;

# An `@` parameter bound to a `$` variable must not write the bare List
# back over the caller's itemized value.
sub takes-pos(@c) { @c.elems }
my $q = (1, 2);
takes-pos($q);
is $q.raku, '$(1, 2)', 'caller $ variable keeps its itemization';
my %h;
%h{$q} = 1;
is %h.keys.elems, 1, 'the $ variable still subscripts as one key';

# Mutations of a container passed through a `$` variable still reach it.
sub push-three(@c) { @c.push(3) }
my $arr = [1, 2];
push-three($arr);
is-deeply $arr, [1, 2, 3], 'push through @ param reaches a $-held Array';
sub assign-all(@c) { @c = 9, 8 }
my $w = [1];
assign-all($w);
is-deeply $w, [9, 8], 'assignment through @ param reaches a $-held Array';
my @b = 1;
assign-all(@b);
is-deeply @b, [9, 8], 'assignment through @ param reaches an @ variable';

# `is copy` copies an itemized List (an Array element) into a mutable Array.
sub set-first(@a is copy) { @a[0] = 1; @a }
my @pool = $(0, 0), $(0, 0);
is-deeply set-first(@pool[0]), [1, 0], 'is copy @ param copies an itemized List';
is-deeply @pool[0], (0, 0), 'the source List is untouched';
sub swap-halves(@x is copy, @y is copy) { @y[0, 1] = @x[0, 1]; @y }
is-deeply swap-halves((1, 2, 3), (4, 5, 6)), [1, 2, 6], 'slice store into an is copy List';
