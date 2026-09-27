use lib 't/lib';
use Test;
use RoleClosureModuleNames;

# A closure created in a ROLE method belongs to the role, not to the class the
# role was composed into. mutsu stamped the composing class onto it, so when
# the closure ran from outside (a Proxy STORE triggered by an assignment in the
# main script) the role's module-internal names resolved against the
# composer's package chain and degraded to plain strings. Found through
# Tinky's `state` Proxy, whose STORE applies a transition that looks up the
# module-internal `ObjectTransitionBefore` role.

plan 4;

class C does Holder {
    method m() is tagged { }
}
my $c = C.new;

is $c.closure()(), 'RoleClosureModuleNames::Tag',
    'a closure from a role method sees the module-internal role';
$c.v = 'x';
is $c.v, '1:x', 'a Proxy STORE from the main script reaches it too';

{
    my class L does Holder {
        method m() is tagged { }
    }
    my $l = L.new;
    is $l.closure()(), 'RoleClosureModuleNames::Tag', 'likewise for a lexical composer';
    $l.v = 'y';
    is $l.v, '1:y', 'and through its Proxy';
}
