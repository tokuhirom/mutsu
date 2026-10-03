use v6;
use Test;
use nqp;

# #11462: rakudo's `Code.name` is `nqp::getcodename($!do)`, so a routine and
# the body in its `$!do` share one name. Renaming the body renames the
# routine, and rebinding `$!do` makes the routine answer the new body's name.

plan 9;

{
    my $s := sub foo { 42 };
    nqp::setcodename(nqp::getattr($s, Code, '$!do'), 'bar');
    is $s.name, 'bar', 'setcodename on $!do renames the routine';
    is $s(), 42, 'the renamed routine still runs';
}

{
    my $r := sub baz { 1 };
    my $o := sub qux { 2 };
    my $do := nqp::getattr($o, Code, '$!do');
    nqp::bindattr($r, Code, '$!do', $do);
    is $r.name, 'qux', 'after rebinding $!do the routine answers the body\'s name';
    is $r(), 2, 'and runs the body';
    ok nqp::eqaddr(nqp::getattr($r, Code, '$!do'), $do),
        'getattr($!do) answers the very body that was bound';
    nqp::setcodename($do, 'zz');
    is $r.name, 'zz', 'renaming the shared body renames the routine';
    is $o.name, 'zz', '... and the routine the body came from';
    $r.set_name('ww');
    is $o.name, 'ww', 'set_name on the rebound routine renames the shared body';
}

{
    # Upstream NativeCall's pattern: bind a replacement's body, then give
    # the body the routine's own name back.
    my $r := sub native-thing { 'old' };
    my $replacement := sub { 'new' };
    my $do := nqp::getattr($replacement, Code, '$!do');
    nqp::bindattr($r, Code, '$!do', $do);
    nqp::setcodename($do, 'native-thing');
    is $r.name, 'native-thing', 'setcodename on the bound body restores the routine name';
}
