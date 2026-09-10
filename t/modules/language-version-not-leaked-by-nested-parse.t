use v6.e.PREVIEW;
use Test;

# A `use vX` pragma is lexical to a compilation unit. Every *nested* parse the
# runtime performs — a module export scan, the module's own source, an EVAL, an
# embedded `{...}` block in a regex — used to leave its own language version
# behind in the parser global, silently dropping the enclosing program back to
# the 6.d default. Version-gated behavior (here: 6.e sprintf flag semantics, the
# cheapest observable probe) then changed halfway through the file.

plan 8;

# Baseline: this unit is 6.e, so the sign precedes the radix prefix.
is sprintf('%#x', -256), '-0x100', '6.e sprintf semantics at unit start';

use lib $?FILE.IO.parent.add('lib').Str;
is sprintf('%#x', -256), '-0x100', '`use lib` does not reset the revision';

use LanguageVersionLeak;
is lang-leak-probe(), 'loaded', 'the pragma-less module loaded';
is sprintf('%#x', -256), '-0x100', 'loading a 6.d module does not reset the revision';

# EVAL inherits the caller's revision (rakudo does the same) ...
is EVAL('sprintf("%#x", -256)'), '-0x100', 'EVAL inherits the caller revision';
# ... and does not leave its own behind.
is sprintf('%#x', -256), '-0x100', 'the revision survives an EVAL';

# The caller's revision wins over a `use vX` inside the EVAL'd string (verified
# against rakudo, which also yields `-0x100` here), and either way that pragma
# must not escape into the enclosing unit.
is EVAL('use v6.d; sprintf("%#x", -256)'), '-0x100', "the caller's revision wins inside EVAL";
is sprintf('%#x', -256), '-0x100', "an EVAL's own pragma does not escape";
