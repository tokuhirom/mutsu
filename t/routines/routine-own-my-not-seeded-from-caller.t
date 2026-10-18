use Test;

# A routine's own `my` variable starts out fresh at every call: a call does
# not seed it from a caller's same-named variable, or from the program-scope
# one it shadows (ADR-12529 §2.3, #13613).

plan 4;

my @log = 'program';
sub inner-array { my @log; @log.push('inner'); @log.elems }
sub outer-array { my @log = 'o'; inner-array() }
is outer-array(), 1, 'an array starts empty';

my %h = a => 1;
sub inner-hash { my %h; %h.elems }
sub outer-hash { my %h = b => 2, c => 3; inner-hash() }
is outer-hash(), 0, 'a hash starts empty';

sub inner-scalar { my $v; $v // 'undef' }
sub outer-scalar { my $v = 'leak'; inner-scalar() }
is outer-scalar(), 'undef', 'a scalar starts undefined';

sub hoisted { my @log = 'own'; peek(); sub peek { @log.join } }
is hoisted(), 'own', 'a nested sub called before its declaration sees the routine array';
