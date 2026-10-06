use v6;
use Test;
use nqp;

# #11462: rakudo's `Code.name` is `nqp::getcodename($!do)`, so a routine and
# the body in its `$!do` share one name. Renaming the body renames the
# routine, and rebinding `$!do` makes the routine answer the new body's name.

plan 17;

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

# #11844: a named sub is reached as `&foo`, and every `&foo` read answered a
# fresh code object built from the routine's registry entry, so a rename
# written into one of them was gone on the next read. The rename now belongs to
# the routine, as it does in rakudo.
{
    sub renamed-by-method { 42 }
    my $before = &renamed-by-method;
    &renamed-by-method.set_name('x');
    is &renamed-by-method.name, 'x', 'set_name on &foo sticks for the next &foo';
    is $before.name, 'x', '... and for an alias taken before the rename';
    ok &renamed-by-method === &renamed-by-method, '... without changing its identity';
    is renamed-by-method(), 42, '... and the routine still runs by its declared name';
    &renamed-by-method.set_name('y');
    is &renamed-by-method.name, 'y', 'a second rename replaces the first';
}

{
    sub renamed-by-nqp { }
    sub untouched { }
    nqp::setcodename(nqp::getattr(&renamed-by-nqp, Code, '$!do'), 'baz');
    is &renamed-by-nqp.name, 'baz', 'setcodename on the $!do of a named sub sticks for the next &foo';
    is &untouched.name, 'untouched', 'another routine keeps its own name';
}

{
    # `.clone` of a routine is a routine of its own.
    sub cloned-original { }
    my $copy = &cloned-original.clone;
    $copy.set_name('copy');
    is &cloned-original.name, 'cloned-original', 'renaming a clone leaves the original alone';
}
