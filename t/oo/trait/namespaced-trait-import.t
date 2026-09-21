use lib 't/lib';
use Test;
use Namespaced::TraitRelay;

plan 1;

class NamespacedTraitImportConsumer {
    has Str $.value is re-export-mark;
}

is NamespacedTraitImportConsumer.^attributes[0].re-export-mark-value,
    'marked',
    'a namespaced module keeps an imported categorical trait in its lexical package';
