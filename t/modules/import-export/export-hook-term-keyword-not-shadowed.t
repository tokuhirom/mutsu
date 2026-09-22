use v6;
use Test;

# Companion to `export-hook-shadows-term-keyword.t`: the same fixture module,
# told through its `use` arguments to install its two values under DIFFERENT
# names. This is what makes the export set genuinely unknowable to a static
# scan of the module -- the names are `@options[0]`/`@options[1]`, run-time
# strings -- and it is the case the run-time resolution must get right in the
# other direction: this compunit is still "tainted" (it imported through a
# `sub EXPORT` hook, so its term keywords are resolved at run time), but
# nothing shadows `True`/`False`, which must therefore answer exactly as they
# do in a file that imports nothing at all.
#
# https://github.com/tokuhirom/mutsu/issues/9047

plan 5;

use lib 't/lib';
use ExportHookShadowsTermKeyword <Yes No>;

is Yes.Str, 'Tri(1)', 'the hook installed its values under the names the `use` arguments chose';
is No.Int, -1, '... both of them';

ok True === Bool::True, '`True` is untouched: nothing shadows it in this compunit';
ok False === Bool::False, '... and neither is `False`';
is (True, False).map(*.Int).join(','), '1,0', 'the unshadowed terms still give the Bool answers';
