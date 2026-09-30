use PluginLoad;
use PluginLoad::BareExporter;
use PluginLoad::UnitExporter;

class PluginLoad::Plugin {
    method bare { plugin-load-bare() }
    method unit { plugin-load-unit() }
}
