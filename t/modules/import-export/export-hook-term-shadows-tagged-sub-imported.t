use Test;
use lib 't/lib';

# #9339, the tagged-import half of export-hook-term-shadows-tagged-sub.t: even
# when `:t` DOES import the tag-exported `sub t`, a bare `t` names the term the
# `sub EXPORT` hook installed. (`t()` calling the sub is blocked on #9389.)

plan 4;

use ExportHookTermVsTaggedSub :t;
is t.hi, 'hi', 'use M :t; a bare t still names the term';
is (t).^name, 'ExportHookTermVsTaggedSub', 'use M :t; a parenthesized bare t is the term';

# A bare tagged sub is still CALLED when the hook installs other names only.
use ExportHookOtherTerm :u;
is u, 'from-sub-u', 'a bare tagged sub is called when the hook installs a different name';
is other, 42, 'the hook-installed term is visible alongside it';
