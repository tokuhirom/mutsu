use v6;
use lib 't/fixtures/role-method-where-sub/lib';
use Test;
use WhereSubRole;

plan 6;

# A role declared in a module whose multi method has a `where` clause naming a
# sub (or a `term:<...>`) of that module, composed into a class of another file.
class Host does WhereSubRole { }
my $h = Host.new;

is $h.plain(10), "big", 'the method body reaches the module sub';
is $h.size(10), "big", 'a where clause reaches the module sub';
is $h.size(1), "small", 'and falls to the other candidate';
is $h.kind(3), "int", 'a where clause reaches a module term';
dies-ok { $h.kind("x") }, 'which rejects what it does not accept';

# Delegation to a handler built from the role (IP::Addr::Handler's shape).
class Wrapper {
    has WhereSubRole $.handler handles *;
    submethod BUILD { $!handler = (class :: does WhereSubRole { }).new }
}
is Wrapper.new.size(10), "big", 'through a delegating attribute';
