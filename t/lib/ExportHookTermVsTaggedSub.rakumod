# Fixture for t/modules/import-export/export-hook-term-shadows-tagged-sub.t (#9339, #9389):
# the shape of Terminal::ANSI::OO -- a tag-exported `sub t` plus a `sub EXPORT`
# hook that installs a sigilless term under the same name.
class ExportHookTermVsTaggedSub {
    method hi { "hi" }
    sub t is export(:t) { "from-sub" }
}
sub EXPORT($t = 't') { %( $t => ExportHookTermVsTaggedSub.new ) }
