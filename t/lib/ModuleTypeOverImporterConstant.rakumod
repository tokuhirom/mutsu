unit module ModuleTypeOverImporterConstant;

our class MtoicRegistry { has @.suites = 1, 2; }

our $body-name = MtoicRegistry.^name;

my MtoicRegistry $REGISTRY = MtoicRegistry.new;

our sub mtoic-suites() is export(:DEFAULT) { $REGISTRY.suites }

our sub mtoic-type-name() is export(:DEFAULT) { MtoicRegistry.^name }

our sub mtoic-shadowed() is export(:DEFAULT) {
    my \MtoicRegistry = 'own term';
    MtoicRegistry
}
