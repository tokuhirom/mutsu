# Fixture for t/modules/import-export/export-hook-term-shadows-tagged-sub.t (#9339, #9389):
# a module whose own methods use the EXPORT-installed bare term, the way
# Terminal::MultiProgress uses Terminal::ANSI::OO's `t`.
use ExportHookTermVsTaggedSub;
unit class ExportHookTermUser;
method go { t.hi }
# A `$t` parameter shares its env key with the imported sigilless `t`; the
# parameter must win (Log::Async's `remove-tap(Tap $t)`, #9389).
method echo($t) { $t }
