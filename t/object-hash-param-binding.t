use Test;

plan 9;

# An object hash (`my %h{Mu}`) keys by the `.WHICH` of the key OBJECT. Binding
# one to a `%`-sigiled parameter used to register that parameter with the
# implicit value type `Any` and NO key type, which both hid the object-hash
# keying from the parameter's own subscripts (they stringified the key object)
# and stripped `key_type` off the CALLER's hash as a side effect.

my class S { }
my class T { }

my %o{Mu};
%o{S} = 7;

sub read-it(%h) { %h{S} }
is read-it(%o), 7, 'an object-hash key reads through a % parameter';

sub keys-of(%h) { %h.keys.map(*.^name).sort.join(',') }
is keys-of(%o), 'S', 'the keys stay key objects through a % parameter';

sub raku-of(%h) { %h.raku }
like raku-of(%o), /'%{Mu}'/, 'the parameter still reports an object hash';
like %o.raku, /'%{Mu}'/, 'and the caller-s hash keeps its key type';

sub add-one(%h) { %h{T} = 8 }
add-one(%o);
is %o{T}, 8, 'a write through a % parameter uses the object key';
is %o.keys.elems, 2, 'and does not collide with the existing key';
is %o.keys.map(*.^name).sort.join(','), 'S,T', 'both keys are key objects';

# A string-keyed shaped hash keeps working through a parameter too: its keys
# are ordinary strings, so no `.WHICH` canonicalisation may creep in.
my Int %typed{Str};
%typed<a> = 1;
sub typed-read(%h) { %h<a> }
is typed-read(%typed), 1, 'a Str-keyed shaped hash still subscripts by string';

# A plain hash must NOT be promoted to an object hash by the parameter binding.
my %plain = a => 1;
sub plain-raku(%h) { %h.raku }
unlike plain-raku(%plain), /'%{'/, 'a plain hash stays a plain hash';
