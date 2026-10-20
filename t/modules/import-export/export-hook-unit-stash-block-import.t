use v6;
use Test;

plan 3;

# A module first loaded by a block-scoped `use` runs over the block's scoped
# env; its `sub EXPORT` must still see the module's own `my constant &name`
# through `UNIT::` (#14115).
use lib 't/lib';

{
    use SelectiveImport14115 "path-exists";
    ok MY::<&path-exists>:exists, 'a constant-backed routine is exported to the block';
    is path-exists("x"), 'ax', 'and callable';
}
{
    use SelectiveImport14115 "path-is-text";
    ok MY::<&path-is-text>:exists, 'a second block importing another name works';
}
