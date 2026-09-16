use v6;
use lib 't/lib';
use Test;

# XML::Class defines its exported attribute traits inside the role that a
# consumer composes. The import remains visible while the consumer's body is
# registered; composing the role must not shadow that trait dispatcher.
use TraitRole;

lives-ok {
    class Consumer does TraitRole::Marked[marked => 'consumer'] {
        has Str $.value is marked;

        class Nested {
            has Str $.value is marked;
        }
    }
    Consumer.new(value => 'ok');
    Consumer::Nested.new(value => 'ok');
}, 'an imported attribute trait remains dispatchable after composing its defining role';

done-testing;
