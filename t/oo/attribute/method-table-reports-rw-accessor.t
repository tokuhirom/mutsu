use Test;

# `.^method_table`/`.^methods` build a `Method` Instance for each public
# attribute's auto-generated accessor, but always stamped `rw => False`
# regardless of whether the attribute itself was declared `is rw` --
# `.^lookup` on the SAME accessor already threaded the real `is_rw` through
# correctly, so the two introspection paths disagreed. This broke
# `Test::Mock` (`roast/packages`-adjacent `Test::Mock.rakumod`, vendored by
# `App::six-pm::SixPM`'s own test suite): it asks each `.^method_table`
# entry's `.rw` to decide whether its generated mock method should itself be
# `is rw`, and a wrongly-`False` accessor made every mocked `is rw` write
# die with X::Assignment::RO on the very next assignment.

plan 3;

class Foo {
    has Str $.name is rw;
    has Str $.id;
}

my %rw = Foo.^method_table.map({ .key => .value.rw });
ok %rw<name>, '.^method_table reports rw=True for an `is rw` attribute accessor';
nok %rw<id>, '.^method_table reports rw=False for a plain (readonly) attribute accessor';

role R {
    has Str $.tag is rw;
}
class Bar does R { }
my %role-rw = Bar.^method_table.map({ .key => .value.rw });
ok %role-rw<tag>, '.^method_table reports rw=True for a role-composed `is rw` attribute accessor';

done-testing;
