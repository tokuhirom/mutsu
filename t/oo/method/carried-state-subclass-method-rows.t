use Test;

# An instance of a user subclass of a built-in class that carries the
# built-in's state answers the built-in's methods through one probe, and a
# method the subclass declares itself wins.
plan 10;

class P is Proc::Async {}
isa-ok P.new('true').command, List, 'a Proc::Async subclass answers .command';
class PO is Proc::Async { method pid { 42 } }
is PO.new('true').pid, 42, 'a Proc::Async subclass method wins';

class S is Supplier {}
isa-ok S.new.Supply, Supply, 'a Supplier subclass answers .Supply';
class SO is Supplier { method emit($x) { "mine" } }
is SO.new.emit(1), 'mine', 'a Supplier subclass method wins';

class C is Channel {}
my $c = C.new;
$c.send(1);
$c.send(2);
$c.close;
is $c.list.join(','), '1,2', 'a Channel subclass answers .list';
class CO is Channel { method elems { 99 } }
is CO.new.elems, 99, 'a Channel subclass method wins';

class IB is IterationBuffer {}
my $ib = IB.new;
$ib.push(1);
$ib.push(2);
is $ib.elems, 2, 'an IterationBuffer subclass answers .push/.elems';
class IBO is IterationBuffer { method push($x) { "my push" } }
is IBO.new.push(1), 'my push', 'an IterationBuffer subclass method wins';

class PR is Promise {}
is PR.kept(3).result, 3, 'a Promise subclass reaches Promise.kept';
class PRO is Promise { method status { "mine" } }
is PRO.new.status, 'mine', 'a Promise subclass method wins';
