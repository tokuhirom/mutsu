use Test;

# `temp Type.method = value` and `temp Type.^meta = value` restore the old value
# when the scope leaves (Red t/23-metamodel-model: `temp Bla.^table = "..."`).

plan 5;

class Cfg {
    my $level = "info";
    method level() is rw { $level }
}

is Cfg.level, "info", "starts at the default";
{
    temp Cfg.level = "debug";
    is Cfg.level, "debug", "temp sets the value";
}
is Cfg.level, "info", "and restores it on scope exit";

sub noisy { temp Cfg.level = "trace"; my $seen = Cfg.level; $seen }
is noisy(), "trace", "temp inside a routine";
is Cfg.level, "info", "restored after the routine returns";
