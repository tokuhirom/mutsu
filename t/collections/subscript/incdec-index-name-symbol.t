use Test;

# `%h{$k}++` / `@a[$i]++` resolve the *variable's* name through the chunk's
# pre-interned constant symbol rather than re-interning the constant string on
# every execution. Every probe the opcode makes on that name -- the declared
# type constraint, the element-container read, the object-hash key-type lane,
# and the write-back -- now takes the symbol, so this pins that each of those
# still answers exactly what the string-keyed spelling answered.

plan 12;

# The plain lane: no constraint of any kind on the name.
my %plain;
%plain<a>++;
%plain<a>++;
is %plain<a>, 2, 'plain hash element increments';

my @arr = 1, 2, 3;
@arr[1]++;
is @arr.join(','), '1,3,3', 'plain array element increments';

# The object-hash key-type lane (`var_hash_key_constraint`): the declared key
# type must survive the increment, so the key is stored as an Int and not
# stringified.
my %keyed{Int};
%keyed{3}++;
%keyed{3}++;
is %keyed{3}, 2, 'object-hash element increments';
is %keyed.keys[0].^name, 'Int', 'object-hash key keeps its declared type across ++';

my %anykey{Any};
%anykey{1}++;
is %anykey.keys[0].^name, 'Int', 'Any-keyed object hash keeps the key type';

# The same lane reached through an *attribute* (`%!conv`), which is not a
# lexical at all -- its key type comes from the class registry, so this arm
# still resolves by name string and must be unaffected.
class Counter {
    has %.conv{Str};
    method bump($k) { %!conv{$k}++ }
    method peek($k) { %!conv{$k} }
}
my $c = Counter.new;
$c.bump('x');
$c.bump('x');
is $c.peek('x'), 2, 'object-hash attribute element increments';
is $c.conv.keys[0].^name, 'Str', 'object-hash attribute keeps its declared key type';

# The typed-container autovivification lane (`var_type_constraint`), which the
# opcode consults when the element write did not land in place.
my BagHash $bag .= new;
$bag<a>++;
$bag<a>++;
is $bag<a>, 2, 'BagHash weight increments';

my MixHash $mix .= new;
$mix<z>++;
is $mix<z>, 1, 'MixHash weight increments';

# A hash reached through a scalar binding shares the caller's container, so the
# increment must be visible through the original name too.
my %shared;
%shared<k>++;
my $alias = %shared;
$alias<k>++;
is %shared<k>, 2, 'increment through a scalar-bound hash is visible at the source';

# A hash mutated from inside a routine that captured it.
my %captured;
sub bump-captured() { %captured<n>++ }
bump-captured();
bump-captured();
is %captured<n>, 2, 'increment of a captured outer hash accumulates';

# Two same-named-key increments on *different* variables must not share state:
# the opcode's per-name symbol is the variable's name, not the element key.
my %left;
my %right;
%left<same>++;
%right<same>++;
%right<same>++;
is "{%left<same>} {%right<same>}", '1 2', 'two hashes with the same key stay independent';
