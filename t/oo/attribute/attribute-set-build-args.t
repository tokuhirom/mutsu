use Test;

# From ecosystem Trait::Env: the closure given to `Attribute.set_build` is
# called as `(object, default)` where default is the attribute's `is default`
# value, else its type object; a deferred `.map` result is the attribute's
# value; `^role_arguments` reads the parameterization of an attribute's type.
plan 6;

my @seen;
multi sub trait_mod:<is>(Attribute $a, :$track!) {
    $a.set_build( -> |c { @seen.push(c.elems); @seen.push(c[1].raku); (1, 2, 3).map({ $_ * 2 }) } );
}

class T { has @.a is track; }
my $t = T.new;
is-deeply @seen, [2, '[]'], 'build closure got (object, default-or-type)';
is-deeply $t.a, [2, 4, 6], 'a lazy .map result fills the @ attribute';

multi sub trait_mod:<is>(Attribute $a, :$one!) {
    $a.set_build( -> |c { @seen.push(c[1].raku); 'x' } );
}
class D { has Str $.n is one is default('7'); has Str $.s is one; }
@seen = ();
D.new;
is-deeply @seen, ['"7"', 'Str'], '`is default` value, else the type object';
@seen = ();
D.new(:n('3'));
is-deeply @seen, ['Str'], 'an attribute passed to new does not call the closure';

my @arguments;
multi sub trait_mod:<is>(Attribute $a, :$inspect!) {
    @arguments.push($a.type.^role_arguments.list);
}
class V { has Int %.h is inspect; has Str @.p is inspect }
is-deeply @arguments, [(Int,).list, (Str,).list], '^role_arguments of an attribute type';
is-deeply Associative[Int].^role_arguments.list, (Int,).list, '^role_arguments of Associative[Int]';
