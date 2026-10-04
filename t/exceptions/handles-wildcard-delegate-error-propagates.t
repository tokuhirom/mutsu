use Test;

# `has A $.x handles *`: an exception thrown BY the delegate's method must
# propagate, and the method must run once. mutsu swallowed the error, fell
# through to later fallbacks and re-ran the delegate (found via
# ORM::ActiveRecord's SQLite adapter `die` inside a delegated ddl method).

plan 5;

my $n = 0;
class Adapter {
    method boom($a, :$k) { $n++; die "boom $a" }
    method fine($a) { $n++; "fine $a" }
}
class Holder { has Adapter $.adapter handles *; }
my $h = Holder.new(adapter => Adapter.new);

$n = 0;
throws-like { $h.boom("x", :k(1)) }, X::AdHoc, :message(/boom/), 'delegate error propagates';
is $n, 1, 'delegated method ran once';

$n = 0;
is $h.fine("y"), 'fine y', 'normal delegation still works';
is $n, 1, 'ran once';

throws-like { $h.nope }, X::Method::NotFound, 'a method no delegate has is still not found';
