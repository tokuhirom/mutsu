use Test;

# A package stash is a Map of its symbols, so `%( ... )` / `.hash` over one or
# several stashes merges their symbols -- the `sub EXPORT { %( A::EXPORT::DEFAULT::,
# B::EXPORT::DEFAULT:: ) }` re-export idiom (ake).

plan 4;

module A { our sub fa is export { 'a' } }
module B { our sub fb is export { 'b' } }

is-deeply %( A::EXPORT::DEFAULT:: ).keys.List, ('&fa',), '%() of one stash';
is-deeply A::EXPORT::DEFAULT::.hash.keys.List, ('&fa',), '.hash of a stash';
is-deeply %( A::EXPORT::DEFAULT::, B::EXPORT::DEFAULT:: ).keys.sort.List, ('&fa', '&fb'),
    '%() of two stashes merges their symbols';
is %( A::EXPORT::DEFAULT::, B::EXPORT::DEFAULT:: )<&fb>(), 'b', 'the merged symbol is callable';
