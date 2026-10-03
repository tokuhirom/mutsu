use Test;

# A `try` block is a `use fatal` scope: a Failure produced inside it is thrown
# there, so `try { return $s.Num }` with a non-numeric `$s` lands in the try
# (which yields Nil) and the routine carries on, rather than returning the
# Failure. Reduced from Template::Jinja2's `float` filter.

plan 6;

sub to-num($v) { try { return $v.Num }; 'fallback' }
is to-num('abc'), 'fallback', 'Failure from a call inside try is thrown, not returned';
is to-num('42'), 42e0, 'a good value is still returned';

sub h { try { return fail('x') }; 'fb2' }
is h(), 'fb2', 'return fail(...) inside try';
sub k { try { return Failure.new('y') }; 'fb3' }
is k(), 'fb3', 'return Failure.new inside try';

sub plain { return Failure.new('z') }
my $r = plain();
isa-ok $r, Failure, 'outside try a Failure is returned as is';
nok $r.handled, 'and stays unhandled';
