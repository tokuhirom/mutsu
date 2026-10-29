use Test;
use lib 't/fixtures/temp-imported-type/lib';
use TempImportedCfg;

# `temp Type.method = value` and `temp Type.^meta = value` restore the old value
# when the scope leaves (Red t/23-metamodel-model: `temp Bla.^table = "..."`).

plan 7;

class Local {
    my $level = "info";
    method level() is rw { $level }
}

is Local.level, "info", "starts at the default";
{
    temp Local.level = "debug";
    is Local.level, "debug", "temp sets the value";
}
is Local.level, "info", "and restores it on scope exit";

sub noisy { temp Local.level = "trace"; my $seen = Local.level; $seen }
is noisy(), "trace", "temp inside a routine";
is Local.level, "info", "restored after the routine returns";

# The same through a type the module exported (Red's `model Bla` is an imported term).
{
    temp Cfg.level = "warn";
    is Cfg.level, "warn", "temp on an imported type object";
}
is Cfg.level, "info", "restored";
