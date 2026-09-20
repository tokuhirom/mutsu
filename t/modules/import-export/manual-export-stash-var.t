use Test;

use lib 't/lib';
use ManualExportStashVarMod :special;

plan 1;

is Answer, 42,
    'a value assigned directly to a tagged EXPORT stash is imported';
