use Test;
use lib $?FILE.IO.parent(3).add('lib');
plan 4;

# A `sub EXPORT` hook that shadows a CORE term (`True`) for one `use` only.
is EVAL('use ExportHookCoreTerm; True'), 'shadowed', 'the first EVAL sees the hook`s term';
is EVAL('use ExportHookCoreTerm <x>; True').raku, 'Bool::True',
    'a later EVAL whose use exports no such term sees the core one';
is EVAL('use ExportHookCoreTerm <x>; True').raku, 'Bool::True',
    'and keeps seeing it';
is EVAL('use ExportHookCoreTerm; True'), 'shadowed', 'a use that does export it sees it again';
