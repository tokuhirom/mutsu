# Fixture for t/modules/import-export/require-import-enum-key.t: an enum
# exported from a module block, as Crypt::SodiumPasswordHash declares it.
module RequireEnumKeys {
    enum Algo is export (AA => 1, BB => 2);
    sub algo-name(Algo $a) is export { $a.key }
}
