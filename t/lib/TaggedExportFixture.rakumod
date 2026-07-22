# A bare-file module (no `unit module`/package block): its exports register
# only under GLOBAL. Used to test that a `use Mod :tag` AFTER a plain `use Mod`
# can still import the tag-only exports (which the plain `use` hid).
sub always-here is export { "always" }
sub only-extra is export(:extra) { "extra" }
sub term:<TAGTERM> is export(:extra) { 99 }
