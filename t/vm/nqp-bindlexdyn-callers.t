use Test;
use nqp;

# nqp::bindlexdyn binds a dynamic its CALLER declares. A routine that takes
# arguments must still be called with its callers' frames visible, and a
# routine's implicit `$_` / `$/` / `$!` always exists in the calling frame,
# assigned or not (#17452). Checked against rakudo.

plan 6;

sub bind-arg($v) { nqp::bindlexdyn('$*x', $v); 1 }
sub with-arg() { my $*x = 0; bind-arg(5); $*x }
is with-arg(), 5, 'a callee that takes arguments binds its caller\'s dynamic';

sub bind-arg-named(:$v) { nqp::bindlexdyn('$*y', $v); 1 }
sub with-named() { my $*y = 0; bind-arg-named(:v(6)); $*y }
is with-named(), 6, 'so does one that takes a named argument';

sub bind-topic($v) { nqp::bindlexdyn('$_', $v); 1 }
sub unassigned-topic() { bind-topic(7) }
lives-ok { unassigned-topic() }, 'binding the caller\'s never-assigned $_ lives';

class C { method m() { bind-topic(8) } }
lives-ok { C.m }, '... from a method caller too';

sub bind-match($v) { nqp::bindlexdyn('$/', $v); nqp::bindlexdyn('$!', $v); 1 }
sub unassigned-match() { bind-match(9) }
lives-ok { unassigned-match() }, 'and the caller\'s never-assigned $/ and $!';

# The pattern Qwiratry's `call-with-topic` uses: save, bind, call, restore.
class Driver {
    method call-with-topic($item, &code) {
        my $saved := try nqp::getlexdyn('$_');
        nqp::bindlexdyn('$_', $item);
        my $result = code($item);
        nqp::bindlexdyn('$_', $saved);
        $result
    }
    method emit(@rows) { @rows.map({ self.call-with-topic($_, -> $x { $x * 2 }) }).List }
}
is-deeply Driver.new.emit([1, 2, 3]), (2, 4, 6), 'save / bind / restore around a body call';
