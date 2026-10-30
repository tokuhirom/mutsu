use Test;

# `method table is rw { $!table //= compute() }`: the compound assignment is the
# routine's tail, and an rw routine hands back the attribute's container, so the
# caller can assign through it (Red MetamodelX::Red::Model.table).

plan 6;

class Named {
    has $!table;
    has $!alt;
    method table() is rw { $!table //= "default" }
    method alt() is rw { $!alt ||= "first" }
}

my $n = Named.new;
is $n.table, "default", "the //= tail still yields the value";
$n.table = "renamed";
is $n.table, "renamed", "and assigning through the rw method sticks";
is $n.alt, "first", "||= tail";
$n.alt = "second";
is $n.alt, "second", "assigning through the ||= tail";

{
    temp $n.table = "scoped";
    is $n.table, "scoped", "temp through the rw method";
}
is $n.table, "renamed", "restored";
