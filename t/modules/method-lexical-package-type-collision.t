use v6;
use Test;

plan 1;

# A method's bare type reference belongs to its declaration package. A caller
# may have a nested class with the same short name, but that caller lexical
# must not leak through the method's scoped environment.
module ScopeProvider {
    class Repository { }
    class Event {
        method repository() { Repository }
    }
}

class ScopeConsumer {
    my class Repository { }

    method run($event) {
        $event.repository.^name
    }
}

is ScopeConsumer.new.run(ScopeProvider::Event.new),
    'ScopeProvider::Repository',
    'method bare types use the declaration package';
