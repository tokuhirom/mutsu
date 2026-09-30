# No `unit` declarator: the exported sub is a package-less top-level routine
# (zef's `Zef::Utils::URI` has exactly this shape).
class PluginLoad::BareExporter { }

sub plugin-load-bare() is export { 'bare-ok' }
