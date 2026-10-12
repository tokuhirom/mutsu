use Test;

# A method names a lexical type of an enclosing scope through its declaration,
# not through whatever its caller's env chain holds: called after the declaring
# block has exited, it still finds the type (#13014).

plan 8;

class R {
    method plugins { [] }
    method cands { my @u = self.plugins; @u }
    method gathered { my @u = eager gather for self.plugins -> @g { take @g }; @u }
}
sub t(&b) { b() }

my ($sub, $qual, $anon, $role, $inner, $bare);
t {
    my class Two { }
    my class Mock::One { }
    my class S is R { method plugins { [[Two.new],] } }
    $sub = S.new;
    my class Q is R { method plugins { [[Mock::One.new],] } }
    $qual = Q.new;
    $anon = R.new but role :: { method plugins { [[Mock::One.new],] } };
    my role RR { method plugins { [[Two.new],] } }
    $role = R.new but RR;
    my class Inner { method mk { Mock::One.new } }
    $inner = Inner.new;
}
{
    my class Plain { }
    my class B { method mk { Plain.new } }
    $bare = B.new;
}

is $sub.cands.raku, '[[Two.new],]', 'a subclass method names a block lexical class';
is $qual.cands.raku, '[[Mock::One.new],]', 'and a qualified one';
is $anon.cands.raku, '[[Mock::One.new],]', 'an anonymous mixin role method';
is $role.cands.raku, '[[Two.new],]', 'a lexical role method';
is $inner.mk.raku, 'Mock::One.new', 'a method of an unrelated lexical class';
is $bare.mk.raku, 'Plain.new', 'a lexical type of a bare block';
is $anon.gathered.raku, '[[Mock::One.new],]', 'called from inside a gather body';
t {
    my class Two { method hi { 'inner Two' } }
    nok $sub.cands[0][0].can('hi'), 'a same-named caller lexical does not replace it';
}
