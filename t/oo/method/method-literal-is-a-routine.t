use Test;

# A `method (...) { ... }` / `submethod (...) { ... }` literal is a Routine in
# raku, not a Block: `return` returns from it and it reports its own type.
# The `anon` spellings mean only "install no symbol", so they are the same
# literal.
#
# Found via the ecosystem parity cluster (tokuhirom/mutsu#7988):
# MetamodelX::Dataclass writes
#   my &call-me = anon submethod call-me(Mu $obj: *%args) { $obj.new(|%args) };
# which reached no branch at all -- the `anon` path knew only `method`, and
# routed that through the anonymous *sub* parser, so the literal had no
# invocant and answered `Sub`.

plan 21;

# --- the type each declarator reports -------------------------------------
is (method ($x) { 1 }).WHAT.^name, 'Method', 'method literal with params is a Method';
is (method { 1 }).WHAT.^name, 'Method', 'bodied method literal is a Method';
is (submethod ($x) { 1 }).WHAT.^name, 'Submethod', 'submethod literal with params is a Submethod';
is (submethod { 1 }).WHAT.^name, 'Submethod', 'bodied submethod literal is a Submethod';

is (anon method ($x) { 1 }).WHAT.^name, 'Method', 'anon method literal is a Method';
is (anon method { 1 }).WHAT.^name, 'Method', 'bodied anon method literal is a Method';
is (anon method foo ($x) { 1 }).WHAT.^name, 'Method', 'named anon method literal is a Method';
is (anon method foo { 1 }).WHAT.^name, 'Method', 'named bodied anon method literal is a Method';
is (anon submethod ($x) { 1 }).WHAT.^name, 'Submethod', 'anon submethod literal is a Submethod';
is (anon submethod { 1 }).WHAT.^name, 'Submethod', 'bodied anon submethod literal is a Submethod';
is (anon submethod foo ($x) { 1 }).WHAT.^name, 'Submethod', 'named anon submethod literal is a Submethod';

is (my method ($x) { 1 }).WHAT.^name, 'Method', 'my method literal is a Method';
is (my submethod ($x) { 1 }).WHAT.^name, 'Submethod', 'my submethod literal is a Submethod';

# A `sub` literal and a pointy block keep the types they had.
is (sub ($x) { 1 }).WHAT.^name, 'Sub', 'sub literal is still a Sub';
is (-> $x { 1 }).WHAT.^name, 'Block', 'pointy block is still a Block';

# --- a Routine is a `return` boundary --------------------------------------
my $ret = method ($x) { return $x * 2; 99 };
is 5.$ret(3), 6, 'return inside a method literal returns from the method';

my $subret = submethod ($x) { return $x + 1; 99 };
is 5.$subret(3), 4, 'return inside a submethod literal returns from the submethod';

# --- the invocant reaches the body -----------------------------------------
my $topic = method () { self };
is 7.$topic, 7, 'a bodied method literal binds self';

my $named-invocant = anon method ($obj: $x) { $obj + $x };
is 10.$named-invocant(5), 15, 'anon method literal binds a user-named invocant';

# The reduced MetamodelX::Dataclass site: an explicitly-typed invocant and a
# slurpy named, installed with `add_method` and called as a method.
class Dataclassish { has $.v }
my &call-me = anon submethod call-me(Mu $obj: *%args) { $obj.new(|%args) };
is &call-me.WHAT.^name, 'Submethod', 'anon submethod with invocant and slurpy is a Submethod';
Dataclassish.^add_method('CALL-ME', &call-me);
is Dataclassish.CALL-ME(v => 4).v, 4, 'the added submethod runs with its declared invocant';
