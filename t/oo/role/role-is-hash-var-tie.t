use v6;
use Test;

# From the Hash::MutableKeys distribution: `role R is Hash` used as a variable
# trait (`my %h is R`, `my %h is R["x"]`), with role methods overriding native
# Hash ones, `callsame` reaching the Hash storage, and `self."$name"(...)`.

plan 11;

role R[$m = "push"] is Hash {
    method keys() { callsame.sort }
    method go($k, $v) { self."$m"($k, $v) }
    method mv($k, $n) { self."$m"($n, self.DELETE-KEY($k)<>) }
}

my %h is R = a => 1, b => 2;
is %h.gist, '{a => 1, b => 2}', 'initializer is stored through the Hash base';
is %h.keys.join(','), 'a,b', 'role method keys wins and callsame reaches the storage';
%h<c> = 3;
is %h.gist, '{a => 1, b => 2, c => 3}', 'subscript assignment keeps the instance';

%h.go('d', 4);
is %h<d>, 4, 'self."$name"(...) mutates the storage (push)';

my %j is R["append"] = a => (1, 2, 3), b => 666;
%j.mv('a', 'foo');
%j.mv('b', 'foo');
is %j.gist, '{foo => [1 2 3 666]}', 'parameterised role trait applies its argument';

my $x = R.new(a => 1);
is $x.gist, '{a => 1}', 'R.new populates the Hash storage';
$x.STORE((b => 2,));
is $x.gist, '{b => 2}', 'STORE replaces the contents';

role Plain is Hash { method k { 'role-method' } }
my %p is Plain = x => 1;
is %p.k, 'role-method', 'a role method is reachable through the variable';
is %p.elems, 1, 'native Hash methods still reach the storage';

role Over is Hash { method elems { 'overridden' } }
my %o is Over = x => 1, y => 2;
is %o.elems, 'overridden', 'a role method overrides a native Hash method';
is %o.keys.sort.join(','), 'x,y', 'other Hash methods are untouched';
