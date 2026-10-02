use Test;

plan 4;

# A CATCH handler that runs for an exception thrown in a deeper routine merges
# its by-name writes back into the installing frame. It decides whether the
# throw site and the installing frame see "the same variable" by comparing
# their bindings. That compare used structural equality, which recursed forever
# when both held same-shaped cyclic object graphs (a tree whose children point
# back at their parent) under one name -- a stack overflow (#11014, found as a
# Template::HAML test "hang" killed by the crash handler's alarm).

class N { has $.parent is rw; has @.children }
sub tree() { my $r = N.new; my $k = N.new(parent => $r); $r.children.push($k); $r }

sub thrower() { my $node = tree(); my &c = { $node }; die "boom" }

sub installing() {
    my $node = tree();
    my &c = { $node };
    my $caught = 'no';
    thrower();
    CATCH { default { my $node = 1; $caught = .message } }
    $caught
}
is installing(), Nil, 'a handler declaring a same-named lexical over cyclic graphs completes';

sub writes-outer() {
    my $node = tree();
    my &c = { $node };
    my $msg;
    {
        thrower();
        CATCH { default { $msg = .message } }
    }
    $msg
}
is writes-outer(), 'boom', 'the handler still writes the installing frame lexical';

# The throws-like shape the bug was found through.
sub render() { my $node = tree(); my &c = { $node }; die X::AdHoc.new(payload => 'bad') }
my $node = tree();
my &c = { $node };
throws-like { render() }, X::AdHoc, 'throws-like around a throw over a cyclic graph';
ok $node.children[0].parent === $node, 'the outer graph is untouched';
