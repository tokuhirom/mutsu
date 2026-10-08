use lib 't/lib';
use Test;
use FinishDataModule;

# `$=finish` inside a module is the module's own `=finish` data section; it was
# `Any` there (only the main program had one), so `$=finish.lines` died.
# Reduced from Locale::Codes::Country, which keeps its data table after
# `=finish`.

plan 3;

is-deeply FinishDataModule.rows, ['alpha:1', 'beta:2', 'gamma:3'],
    'module reads its own $=finish at load time';
is FinishDataModule.late, 3, 'a module routine reads $=finish after the load';
ok !$=finish.defined, 'a script without =finish does not see the module section';
