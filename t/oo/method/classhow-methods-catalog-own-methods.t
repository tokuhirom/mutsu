use Test;

plan 8;

# #13494: .^methods lists the methods a built-in class declares itself, also
# for the classes whose catalog owner used to be unknown to the enumeration.
sub names(Mu \t) { t.^methods.map(*.name).Set }

ok <key value antipair>.all (elem) names(Pair), 'Pair.^methods lists its own methods';
ok <keys values pairs kv of>.all (elem) names(Map), 'Map.^methods lists its own methods';
ok <elems keys grab>.all (elem) names(Set), 'Set.^methods lists its own methods';
ok <elems keys grab>.all (elem) names(Bag), 'Bag.^methods lists its own methods';
ok <keys values>.all (elem) names(Capture), 'Capture.^methods lists keys and values';
ok <keys values antipairs>.all (elem) names(Hash), 'Hash.^methods also lists the Map methods it inherits';
nok 'uc' (elem) names(Pair), 'Pair.^methods does not list Cool text methods';
ok Pair.^methods.grep(*.name eq 'key').elems == 1, 'a name is listed once per class';
