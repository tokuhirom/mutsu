use Test;

# A signature literal's parameter defaults are compiled when `.params` builds
# its Parameter objects. A hash/array literal default must compile (an
# `Expr::Spelled` wrapper left on it used to panic a debug build).
plan 8;

my $s = :(%a = {:a(2)}, @c = [1, 2], :%b = {:a(2)}, :@d = [3]);
my @p = $s.params;
is @p.elems, 4, "four parameters";
is @p[0].default.().raku, '{:a(2)}', "hash default";
is @p[2].default.().raku, '{:a(2)}', "named hash default";
is @p[1].default.().raku, '[1, 2]', "array default";
is @p[3].default.().raku, '[3]', "named array default";

my $w = :($x where { $_ > 1 }, Int $y where {%(a => 1){'a'} == $_});
is $w.params.elems, 2, "where clauses with hash subscripts";
ok $w.params[0].constraint_list.elems >= 0, "constraint list is reachable";

is :(&a = &say, :&b = &say).params.elems, 2, "code defaults";
