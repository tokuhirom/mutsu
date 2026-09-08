unit module BlockUseNestedInner;
use NativeCall;

# `Inner`'s OWN view of `&nativecast`, not the script's. A `unit module` body
# runs at `current_package() == GLOBAL`, so this import registers
# `GLOBAL::nativecast` -- an entry an enclosing block's import-scope pop used to
# drop. See `t/block-use-keeps-nested-module-imports.t`.
sub inner-probe() is export { defined(&nativecast) ?? 'visible' !! 'MISSING' }
