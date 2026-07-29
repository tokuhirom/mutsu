use v6;
use Test;

# A hash attribute initialized from a Map must accept element assignments.
# The Map's embedded container metadata (declared_type, no value type)
# rendered as an EMPTY element constraint, which rejected every assignment
# with "Type check failed for an element of %; expected  but got ...".
# DBDish::Pg::Connection does exactly this:
#
#     has %.dynamic-types = %oid-to-type;    # a Map constant
#     ...
#     $dbh.dynamic-types{$oid} = SomeType;   # t/36-pg-enum.rakutest
#
# which died at that assignment (13 of 26 tests ran).

plan 4;

class C {
    has %.h = Map.new(1 => Int, 2 => Str);
}

my $c = C.new;
lives-ok { $c.h{3} = Str }, 'type-object element assignment into Map-defaulted hash attr';
lives-ok { $c.h{4} = 42 }, 'plain value element assignment into Map-defaulted hash attr';
is $c.h.elems, 4, 'both elements landed';
is $c.h{4}, 42, 'assigned value reads back';
