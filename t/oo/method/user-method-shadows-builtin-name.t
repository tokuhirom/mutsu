use Test;

plan 6;

# A method the user declared on a class outranks a same-named builtin. The
# runtime's by-name dispatchers key on the method name alone and used to answer
# first, so a class method called `map`, `elems`, `sort`, ... was unreachable.
# The shape that surfaced it: a grammar action class dispatches the action named
# after each rule, so a grammar with a rule called `map` (YAMLish's block-mapping
# rule) needs `Actions.map($/)` to reach the action and not the collection
# builtin, which rejected the Match with "Cannot map a Match ..., it's not
# callable".

grammar RuleNamedMap {
    token TOP   { <map> }
    token map   { <entry>+ % ',' }
    token entry { $<k>=[\w+] '=' $<v>=[\d+] }
}
class MapActions {
    method TOP($/)   { make $<map>.ast }
    method map($/)   { make ['MAP', @<entry>».ast] }
    method entry($/) { make ~$<k> => +$<v> }
}

my $m = RuleNamedMap.parse('a=1,b=2', :actions(MapActions));
ok $m.defined, 'a grammar rule named `map` parses with actions';
is $m.ast.raku, ['MAP', [a => 1, b => 2]].raku, 'its action method ran, not the builtin .map';

# The same shadowing, without grammars, on an instance and on the type object.
class Shadower {
    method map($x)  { "user-map($x)" }
    method elems    { 'user-elems' }
}
is Shadower.new.map(7), 'user-map(7)', 'an instance reaches the user `map`';
is Shadower.map(7),     'user-map(7)', 'the type object reaches the user `map`';
is Shadower.new.elems,  'user-elems',  'a user `elems` outranks the builtin';

# A class that does NOT declare the name still gets the builtin.
class Plain { has @.items }
is Plain.new(items => [1, 2, 3]).items.map(* * 2).join(','), '2,4,6',
    'the builtin .map is untouched where nothing shadows it';
