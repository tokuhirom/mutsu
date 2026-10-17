use Test;

# ADR-11276 §9.147: the last copies of the array mutators are gone. A `pop`
# the earlier entries left over, and an array attribute of a `Proxy`
# subclass, reach the `Array` rows through a detached place (`invoke_mut`).

plan 12;

class Plain { }
throws-like { Plain.new.pop }, X::Method::NotFound, 'pop of an object with no pop is no method';

sub fresh { [1, 2, 3] }
is fresh().pop, 3, 'pop of an array held by value';
my $held = [1, 2, 3];
is $held.pop, 3, 'pop through a scalar';
is $held.elems, 2, 'the scalar\'s array lost its last element';
my %h = a => [1, 2];
is %h<a>.pop, 2, 'pop of a hash element';
is %h<a>.elems, 1, 'the element array lost it';

class LoggedProxy is Proxy { has @.log }
my $p := LoggedProxy.new(FETCH => method () { 42 }, STORE => method ($v) { });
$p.VAR.log.push(1, 2);
$p.VAR.log.unshift(0);
is $p.VAR.log.join(','), '0,1,2', 'push and unshift on a Proxy subclass attribute';
is $p.VAR.log.pop, 2, 'pop on a Proxy subclass attribute';
is $p.VAR.log.shift, 0, 'shift on a Proxy subclass attribute';
$p.VAR.log.append((5, 6));
is $p.VAR.log.join(','), '1,5,6', 'append flattens on a Proxy subclass attribute';
$p.VAR.log.prepend((-1, 0));
is $p.VAR.log.join(','), '-1,0,1,5,6', 'prepend flattens on a Proxy subclass attribute';
is $p, 42, 'the proxy still fetches';
