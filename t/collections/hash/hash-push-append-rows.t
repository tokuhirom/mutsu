use Test;
use MONKEY-TYPING;

# `Hash.push` and `Hash.append` are rows of the one method table (ADR-11276
# §9.23): `Handler::Mut` rows that write through the receiver's shared node and
# read the variable's declared types by name when the receiver has one. The one
# row answers every receiver the five former copies served: a named `%h`, a
# `:=`-bound alias, a scalar holding a hash, a by-value hash and an attribute.

plan 26;

my %h;
%h.push('a' => 1);
%h.push('a' => 2);
%h.push((b => 5), (c => 6));
is-deeply %h<a>, [1, 2], 'a repeated key stacks the values';
is %h<b> + %h<c>, 11, 'several pairs are merged';

%h.append('a' => (7, 8));
is-deeply %h<a>, [1, 2, 7, 8], 'append flattens an Array value into the stack';

my %g;
%g.push(<k v>);
is-deeply %g, {k => 'v'}, 'an alternating key, value list';

my %n;
%n.push((x => [1, 2]), (x => [3]));
is-deeply %n<x>, [1, 2, [3]], 'push nests an Array value';
my %m;
%m.append((y => [1, 2]), (y => [3]));
is-deeply %m<y>, [1, 2, 3], 'append flattens it';

my @pairs = (a => 1), (b => 2);
my %p;
%p.push(@pairs);
is %p.sort.gist, '(a => 1 b => 2)', 'a list of pairs';

# the one-argument-rule is not a named argument: an adverb is ignored
my %z;
is-deeply %z.push(:zzz), {}, 'an undeclared named argument is not an element';

# the result is the hash itself
my %r;
%r.push('k' => 1);
ok (%r.push('j' => 2)) === %r, 'push answers the invocant';

# typed hashes check each pushed value, and a repeat of a scalar-typed key
my Int %t;
%t.push('a' => 1);
throws-like { %t.push('b' => 'x') }, X::TypeCheck::Assignment, 'a wrong value type is refused';
throws-like { %t.push('a' => 2) }, X::TypeCheck::Assignment,
    'a repeated key would make the Int value an Array';
is-deeply %t, (my Int %= :a(1)), 'and the hash is left as it was';

# object hashes check the key and store it under its WHICH
my %o{Any};
%o.push(1 => 'a');
is %o.keys[0].WHAT.gist, '(Int)', 'an object hash keeps the key object';
my Int %ob{Rat};
throws-like { %ob.push('x' => 1) }, X::TypeCheck, 'a wrong key type is refused';
%ob.push(0.5 => 1);
is %ob.elems, 1, 'a key of the declared type is stored';

# every way a name can hold the hash
my %src = a => 1;
my $r := %src;
$r.push('z' => 3);
is %src.sort.gist, '(a => 1 z => 3)', 'a scalar bound to the hash writes through';

sub addto(%x) { %x.push('q' => 9) }
my %w;
addto(%w);
is-deeply %w, {q => 9}, 'a hash passed to a routine (a shared cell)';

my $sh = {a => 1};
$sh.push('a' => 2);
is-deeply $sh, {a => [1, 2]}, 'a scalar holding a hash';

sub mk { state %s = (k => 1); %s }
mk().push('k' => 5);
is-deeply mk(), {k => [1, 5]}, 'a by-value receiver is the same container';
is-deeply ({a => 1}).push('a' => 4), {a => [1, 4]}, 'a literal hash';

class C {
    has %.h;
    method add($k, $v) { %!h.push($k => $v); self }
}
my $c = C.new;
$c.add('p', 1).add('p', 2);
is-deeply $c.h, {p => [1, 2]}, 'an attribute';

class H is Hash { }
my $hh = H.new;
$hh.push('a' => 1);
$hh.push('a' => 2);
is-deeply $hh<a>, [1, 2], 'a user subclass of Hash';

# the element is itemized at the store (ADR-0040)
my %i;
%i.push('a' => [1, 2]);
is %i<a>.raku, '$[1, 2]', 'an Array value is itemized';

# a never-written package hash vivifies to an Array: the Array mutators take it
our %pk;
%pk.push('a' => 1);
is-deeply %pk, {a => 1}, 'a package hash';

# the augmented method wins over the row
augment class Hash {
    method append-twice($k, $v) { self.append($k => $v); self.append($k => $v) }
}
my %aug;
%aug.append-twice('a', 1);
is-deeply %aug, {a => [1, 1]}, 'a method augmented onto Hash reaches the rows';

throws-like { Hash.push('a' => 1) }, Exception, 'a type object is not a hash';

done-testing;
