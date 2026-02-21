use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 3;

# Verify make-temp-file is callable as a bareword (loaded from Test::Util module)
{
    ok &make-temp-file.defined, 'make-temp-file is available as an exported function';
}

# Verify make-temp-path is callable as a bareword (alias for make-temp-file)
{
    ok &make-temp-path.defined, 'make-temp-path is available as an exported function';
}

# Verify make-temp-dir is callable as a bareword
{
    ok &make-temp-dir.defined, 'make-temp-dir is available as an exported function';
}
