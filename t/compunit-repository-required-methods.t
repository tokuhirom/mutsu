use Test;

# The CompUnit::Repository role requires exactly `id`, `need`, and `loaded`
# (matching Rakudo). `load` is NOT a required method — a class doing the role
# without a `load` method must still compose.

plan 4;

lives-ok {
    my $cur = (class :: does CompUnit::Repository {
        method need { }; method loaded { }; method id { }
    }).new;
    $cur;
}, 'class with id/need/loaded composes CompUnit::Repository (no load required)';

throws-like {
    (class :: does CompUnit::Repository { method need { }; method loaded { } }).new;
}, X::Role::Composition::Unimplemented, 'missing id is still required';

throws-like {
    (class :: does CompUnit::Repository { method loaded { }; method id { } }).new;
}, X::Role::Composition::Unimplemented, 'missing need is still required';

throws-like {
    (class :: does CompUnit::Repository { method need { }; method id { } }).new;
}, X::Role::Composition::Unimplemented, 'missing loaded is still required';
