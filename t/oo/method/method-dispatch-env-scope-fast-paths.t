use Test;

# Method dispatches that skip the scoped-env flatten (#9494): a user method
# reached through the plain-method lane, `.defined` on a type object, the
# plain-array mutators, and a literal attribute default. Each must leave the
# caller's lexicals, closures and pseudo-stashes exactly as the full path did.

plan 16;

class C {
    has Str $.text;
    has @.items;
    has Bool $.flag is default(False);
    method undefined { !$!text.defined }
    method add(Str $s) { $!text ~= $s; self }
    method keep($x) { @!items.push($x); @!items.elems }
}

sub caller-scope {
    my $outer = 'outer';
    my $c = C.new;
    my @seen;
    for ^3 {
        @seen.push($c.undefined);
        $c.add('x');
    }
    my &peek = { $outer };
    is @seen.join(','), 'True,False,False', 'type-object .defined, then a defined Str';
    is $c.text, 'xx' ~ 'x', 'method writes through the attribute cell';
    is peek(), 'outer', 'a closure made after the calls still sees the caller lexical';
    is MY::<$outer>, 'outer', 'the caller pseudo-stash still holds its lexical';
    $outer = 'changed';
    is peek(), 'changed', 'and the closure tracks a later write';
    $c.keep($_) for 1..3;
    is $c.items.join(','), '1,2,3', 'attribute array push from a method';
}
caller-scope();

class D {
    method defined { False }
}
is D.defined, False, 'a user defined method on the type object still wins';
is D.new.defined, False, 'and on the instance';

class E { }
is E.defined, False, 'type object .defined is False';
is E.new.defined, True, 'instance .defined is True';

sub array-mutators {
    my @a = 1, 2;
    my $before = 'kept';
    @a.push(3);
    @a.unshift(0);
    @a.append(4, 5);
    my $p = @a.pop;
    my $s = @a.shift;
    is @a.join(','), '1,2,3,4', 'push/unshift/append/pop/shift in a routine';
    is "$p,$s", '5,0', 'pop and shift return the removed elements';
    is $before, 'kept', 'an unrelated lexical is untouched';
}
array-mutators();

my @c = 1, 2;
class F { has @.x = @c; has $.n = 0; has Bool $.b is default(True) }
my $f = F.new;
@c.push(3);
is $f.x.join(','), '1,2', 'a non-literal default is still copied, not aliased';
is $f.n, 0, 'a literal default';
is F.new(n => 5).n, 5, 'an explicit value overrides the literal default';
