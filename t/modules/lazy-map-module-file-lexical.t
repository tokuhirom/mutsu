use lib 't/lib';
use Test;

# A lazy `.map`/`.grep` Seq a module routine returns runs its callback when
# the importer consumes it; the callback still reads the module's own
# file-scope lexicals (Text::T9's `my constant %default-keys`).

use LazyMapFileLexical;

plan 7;

my %keys = k => 99;    # an importer lexical of the same name must not answer

is mapped(<k i>).join(','), '5,4', 'a pointy-block map';
is bare-block(<k i>).join(','), '5,4', 'a bare-block map';
is-deeply plain-hash(<k i>).List, (5, 4), 'a plain `my %` hash';
is-deeply grepped(<k x i>).List, <k i>, 'a grep';
is-deeply nested(<ki kk>).List, ('54', '55'), 'a map nested in a lazy map';
is t9(5477, <kiss kssi skis>).join('|'), 'kiss', 'Text::T9 shape';
is %keys<k>, 99, 'the importer lexical is untouched';
