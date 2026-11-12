use Test;
use lib 't/lib-declare-meta-method';
use DeclareRecord;

# A metamethod (`method ^name`) of a DECLARE'd type reaches its HOW's
# `add_meta_method(type, name, method)`, and a `.wrap` made there is what a
# `.^name` call runs (#17513: Red keeps a model's custom `^populate` and wraps
# it to accept fixture rows).
plan 4;

record Item {
    method ^populate($model) { "custom {$model.^name}" }
    method ^kind($model) { 'item' }
    method plain { 'plain' }
}

is Item.^populate, 'custom Item', 'the metamethod without rows runs the declared body';
is Item.^populate(:rows([1, 2, 3])), 'rows: 3', 'the wrapper the HOW installed takes the call with rows';
is Item.^kind, 'item', 'an unwrapped metamethod still dispatches';
is-deeply Item.HOW.added.sort.List, <kind populate>, 'add_meta_method saw every metamethod, and no plain method';
