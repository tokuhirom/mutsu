use Test;

# `.^method_table` of a core value type with no registry class definition
# (Int, Str, Num, Array, Hash, ...) is not empty: it lists the type's own
# methods, drawn from the same source as `.^methods(:local)` (#9388). rakudo
# keeps submethods (`POPULATE`, `BUILD`) out of the table, so the table is a
# subset of `.^methods(:local)` rather than equal to it.

plan 13;

for Int, Str, Num, Array, Hash -> \T {
    my @table = T.^method_table.keys;
    my %local = T.^methods(:local).map({ .name => True });
    ok @table >= 20, "{T.^name}.^method_table is populated";
    ok @table.grep({ !%local{$_} }).elems == 0,
        "{T.^name}.^method_table lists only methods(:local) entries";
}

ok Int.^method_table<abs>:exists, 'Int.^method_table has abs';
is Int.^method_table<abs>(-3), 3, 'a method_table entry is callable';
ok Str.^method_table<uc>:exists, 'Str.^method_table has uc';
