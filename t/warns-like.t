use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 5;

# Basic: code warns and message matches regex
warns-like 'warn "hello"', /hello/, 'warns hello matches regex';

# Warn with multi-char match
warns-like 'warn "testwarning"', /testwarning/, 'matches full warning text';

# Warn with partial regex
warns-like 'warn "something-went-wrong"', /wrong/, 'partial regex match';

# Multiple warns - `warns-like` resumes each one and keeps the LAST message
# (its CONTROL handler overwrites `$message` per warning), so asserting on the
# first one fails under rakudo too.
warns-like 'warn "first"; warn "second"', /second/, 'last warning captured';

# Warn inside a block
warns-like 'for 1..1 { warn "loopwarn" }', /loopwarn/, 'warn inside loop';
