use v6;
use Test;

plan 9;

# Rakudo keeps `^add_multi_method` candidates in multi_methods_to_incorporate
# until `compose`, so `^can` does not see them before then. A metamodel that
# asks `^can` before adding its own accessor (Red) relies on it.
my \T = Metamodel::ClassHOW.new_type(:name<PendingMulti>);
ok !T.^can("m"), 'no such method before anything is added';
T.^add_multi_method("m", my method (::U Mu:U:) { "undefined" });
ok !T.^can("m"), 'a pending multi candidate is not visible to ^can';
T.^add_method("plain", my method () { "plain" });
ok T.^can("plain"), 'a plain added method is visible at once';
T.^compose;
ok T.^can("m"), 'the multi candidate is visible once composed';
is T.m, "undefined", 'and it dispatches';

# The check-then-add idiom: both candidates end up in one family.
my \U = Metamodel::ClassHOW.new_type(:name<TwoCandidates>);
U.^add_multi_method("name", my method (::V Mu:U:) { "type object" });
U.^add_multi_method("name", my method (Mu:D:) { "instance" }) unless U.^can("name");
U.^compose;
is U.name, "type object", 'the :U candidate answers the type object';
is U.new.name, "instance", 'the :D candidate was added too';

# An inherited method keeps `^can` true.
class Base { method inherited() { 1 } }
my \W = Metamodel::ClassHOW.new_type(:name<PendingChild>);
W.^add_parent(Base);
W.^add_multi_method("inherited", my method (::X Mu:U:) { 2 });
ok W.^can("inherited"), 'an inherited method of the same name stays visible';
W.^compose;
ok W.^can("inherited"), '... also after compose';
