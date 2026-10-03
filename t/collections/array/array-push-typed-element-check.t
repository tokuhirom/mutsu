use Test;

# The growing array mutators on a typed container take the native fast path
# when no argument is Nil (#9494). The element check, the Nil decay and the
# container's type must all still behave as on the general path.

plan 11;

my Int @a;
@a.push(1);
@a.push(2, 3);
@a.append(4, 5);
@a.unshift(0);
@a.prepend(-1);
is @a.raku, 'Array[Int].new(-1, 0, 1, 2, 3, 4, 5)', 'push/append/unshift/prepend on Int @a';
throws-like { @a.push('x') }, X::TypeCheck::Assignment, 'an ill-typed push still dies';
is @a.elems, 7, 'and leaves the array unchanged';
@a.push(Nil);
is @a.raku, 'Array[Int].new(-1, 0, 1, 2, 3, 4, 5, Int)', 'a pushed Nil decays to the element type';

my int @n;
@n.push(7);
@n.append(8, 9);
is @n.raku, 'array[int].new(7, 8, 9)', 'a native int array';

class F { has $.v }
class R {
    has F @.fields;
    method add($f) { @!fields.push: $f; self }
}
my $r = R.new;
$r.add(F.new(v => 1)).add(F.new(v => 2));
is $r.fields.elems, 2, 'typed attribute array push from a method';
is $r.fields[1].v, 2, 'the pushed element';
throws-like { $r.add(42) }, X::TypeCheck::Assignment, 'an ill-typed attribute push dies';
is $r.fields.elems, 2, 'and leaves the attribute unchanged';
is $r.fields.of.^name, 'F', 'the attribute keeps its element type';

sub s { my Str @s; @s.push('a'); @s.push('b'); @s }
is s().raku, 'Array[Str].new("a", "b")', 'a typed lexical array in a routine';
