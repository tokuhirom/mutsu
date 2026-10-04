use lib 't/lib';
use Test;
use DoBindShadowOuter;

# A `$!do` bound on one module's routine runs for that routine only, not for
# a same-named routine another package declares (`Compress::Zlib`'s local
# `compress` wrapper around `Compress::Zlib::Raw`'s native `compress`).

plan 2;

is call-local(), 'local(1)', 'the local same-named sub keeps its own body';
is call-inner(), 'replaced(3)', 'the bound routine runs its bound body';
