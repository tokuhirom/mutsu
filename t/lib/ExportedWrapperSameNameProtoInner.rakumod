# A bare-file (no `unit module`) dependency exporting a `proto`/`multi`
# family under the same name an importer's own exported wrapper uses
# (#8798). Its own subs are only ever installed under `GLOBAL::`, exactly
# like `P5getgrnam`'s `getgrgid`/`getgrnam`/`getgrent` wrappers.
my proto sub wrapped-thing(|) is export {*}
multi sub wrapped-thing(Int $n) {
    "raw:$n";
}
multi sub wrapped-thing(Str $s) {
    "raw:$s";
}
