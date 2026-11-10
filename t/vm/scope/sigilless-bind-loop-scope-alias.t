use Test;

plan 1;

# A loop body's `my \value = $pair.value` must not write through into the
# caller's hash when enclosing routines also name a sigilless `value`.
sub sp(\object) { object.pairs }
sub eo(\object) {
	for sp(object) -> $pair {
		my \value = $pair.value;
	}
}
sub em(\value) { eo(value) }
sub tw(\value) {
	my $mode = 0;
	em(value);
}
my %h = a => 1, b => 2;
tw(%h);
is %h.raku, '{:a(1), :b(2)}', 'hash survives nested sigilless value binds';

# vim: expandtab shiftwidth=4
