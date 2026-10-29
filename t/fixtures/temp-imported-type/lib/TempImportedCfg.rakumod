unit module TempImportedCfg;

class Cfg is export {
    my $level = "info";
    method level() is rw { $level }
}
