use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 4;

# Basic: same path
is-path "/tmp".IO, "/tmp".IO, "identical paths match";

# Path with .. that resolves to the same location
is-path "/tmp/../tmp".IO, "/tmp".IO, "path with .. resolves to same";

# Path with trailing slash vs without
is-path "/tmp/".IO, "/tmp".IO, "trailing slash resolves to same";

# Relative path that resolves to cwd
is-path ".".IO, $*CWD.IO, "dot resolves to cwd";
