use Test;

# #8746: a module's `sub EXPORT` may internally `use` another module and
# re-export one of its `is export` subs wrapped in a closure under the SAME
# bare name it wraps (a common dependency-injection idiom). A bareword call
# of the exported name must run the wrapper `sub EXPORT` installed into
# `env`, not the plain routine of the same name the inner module's `use`
# left registered under the flat GLOBAL registry.
plan 1;

use lib 't/lib';
use ExportWrapperShadowFixture;

is export-wrapper-shadow-greet(), 'hello from wrapped',
    'a bareword call runs the EXPORT-installed wrapper, not the unwrapped inner routine';
