# Loaded only through a `require` inside a sub of
# t/modules/import-export/require-in-routine-file-scope-lexical.t, and reached
# through a punned role (#18126).
my $lock = Lock.new;
my $cache = Map.new;
sub p1() { $lock.protect({ $cache.^name }) }
sub p2() { $lock.protect({ my $x = $cache; $x.^name }) }
sub p3() { $lock.protect({ $cache }).^name }
sub p4() { $cache.^name }
role RequireInRoutineLexRole {
    method p1 { p1() }
    method p2 { p2() }
    method p3 { p3() }
    method p4 { p4() }
}
