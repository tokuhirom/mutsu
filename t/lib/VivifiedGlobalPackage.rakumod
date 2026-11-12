# Fixture for t/modules/import-export/export-hook-resolve-lexical-constant.t
# (#15371): a package vivified under GLOBAL at BEGIN time, the way
# CompUnit::Util's `set-unit('GLOBALish::Foo::Bar', ...)` does.
BEGIN {
    my $W := GLOBAL.WHO;
    $W.package_at_key('VivifiedPkg');
    $W<VivifiedPkg>.WHO<$answer> = 42;
}
