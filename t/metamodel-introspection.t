use Test;

plan 45;

# ---------------------------------------------------------------------------
# Metamodel::MethodContainer -- .^lookup / .^find_method / .^can
# ---------------------------------------------------------------------------

class Looked {
    method present() { 'p' }
    method !hidden() { 'h' }
    multi method multi-m(Int) { 'i' }
    multi method multi-m(Str) { 's' }
}

is Looked.^lookup('present').name, 'present', '.^lookup finds a declared method';
ok Looked.^lookup('present') ~~ Method, '.^lookup answers a Method object';
is Looked.^lookup('multi-m').name, 'multi-m', '.^lookup finds a multi dispatcher';

# An absent name answers the Mu TYPE OBJECT, not Nil.
ok Looked.^lookup('no-such-method') === Mu, '.^lookup of an absent name is Mu';
is Looked.^lookup('no-such-method').^name, 'Mu', '.^lookup absent name .^name is Mu';
nok Looked.^lookup('no-such-method').defined, '.^lookup absent name is undefined';
ok Looked.^find_method('no-such-method') === Mu, '.^find_method of an absent name is Mu';

# A private method is not reachable through .^lookup.
ok Looked.^lookup('hidden') === Mu, '.^lookup does not see a private method';

# An inherited method is.
is Looked.^lookup('say').name, 'say', '.^lookup walks the MRO';

ok Looked.^can('present').elems >= 1, '.^can finds a declared method';
is Looked.^can('present')[0].name, 'present', '.^can answers Method objects';
is Looked.^can('no-such-method').elems, 0, '.^can of an absent name is empty';
ok Looked.^can('say').elems >= 1, '.^can walks the MRO';

# ---------------------------------------------------------------------------
# A metaobject passed as the introspected object stands for the type its
# receiver describes -- Rakudo's HOW methods read `self`'s own MRO and method
# table and ignore the `$obj` argument.
# ---------------------------------------------------------------------------

{
    my $any-object = 'random object';
    my $metadata = $any-object.HOW;

    is $metadata.^name, 'Perl6::Metamodel::ClassHOW', 'a HOW reports its own metaclass';
    is $metadata.^mro.map(*.^name).join(' '),
       'Perl6::Metamodel::ClassHOW Any Mu',
       '.^mro on a HOW is the metaclass MRO';

    my @can = $metadata.can($metadata, 'uc');
    ok @can.elems >= 1, '.can with the metaclass itself as invocant is not empty';
    is @can[0].name, 'uc', '.can with a HOW invocant resolves against the described type';

    is $metadata.name($metadata), 'Str', '.name with a HOW invocant names the described type';
    is $metadata.mro($metadata).map(*.^name).join(' '), 'Str Cool Any Mu',
       '.mro with a HOW invocant walks the described type';
    is $metadata.lookup($metadata, 'uc').name, 'uc',
       '.lookup with a HOW invocant searches the described type';

    # The ordinary form keeps working.
    is $any-object.HOW.can($any-object, 'uc')[0].name, 'uc',
       '.can with the plain instance as invocant still works';
}

# ---------------------------------------------------------------------------
# Metamodel::Versioning -- .^ver / .^auth / .^api and their setters
# ---------------------------------------------------------------------------

class Unversioned { }
ok Unversioned.^ver === Mu, 'an undeclared .^ver is the Mu type object';
is Unversioned.^auth, '', 'an undeclared .^auth is the empty string';
is Unversioned.^api, '', 'an undeclared .^api is the empty string';

class Declared:ver<1.2.3>:auth<github:someone>:api<2> { }
is Declared.^ver, v1.2.3, ':ver<...> is readable through .^ver';
is Declared.^ver.^name, 'Version', '.^ver answers a Version object';
is Declared.^auth, 'github:someone', ':auth<...> is readable through .^auth';
is Declared.^api, '2', ':api<...> is readable through .^api';

class Versioned { }
Versioned.^set_ver: v0.0.1;
Versioned.^set_auth: 'github:mutsu';
Versioned.^set_api: '3';
is Versioned.^ver, v0.0.1, '.^set_ver is readable back through .^ver';
is Versioned.^ver.^name, 'Version', '.^set_ver stores a Version object';
is Versioned.^auth, 'github:mutsu', '.^set_auth is readable back through .^auth';
is Versioned.^api, '3', '.^set_api is readable back through .^api';

# ---------------------------------------------------------------------------
# Metamodel::Documenting -- .^set_why stays legal after .^compose, because it
# mutates the METACLASS rather than the composed type object.
# ---------------------------------------------------------------------------

BEGIN {
    our Mu constant Documented = Metamodel::ClassHOW.new_type: :name<Documented>;
    Documented.HOW.compose: Documented;
    Documented.HOW.set_why: do {
        my Pod::Block::Declarator:D $pod .= new;
        $pod._add_leading:  'Documented is an example class.';
        $pod._add_trailing: 'Take a look at my WHY!';
        $pod
    };
}

is Documented.HOW.WHY.Str,
   "Documented is an example class.\nTake a look at my WHY!",
   '.HOW.set_why survives .HOW.compose and reads back through .HOW.WHY';
is Documented.WHY.Str,
   "Documented is an example class.\nTake a look at my WHY!",
   'the type object reports the same .WHY as its metaclass';
is Documented.^name, 'Documented', 'a hand-composed type keeps its name';

{
    # `_add_leading` / `_add_trailing` accumulate space-joined.
    my $pod = Pod::Block::Declarator.new;
    $pod._add_leading('one');
    $pod._add_leading('two');
    is $pod.leading, 'one two', '_add_leading accumulates space-joined';
    $pod._add_trailing('three');
    is $pod.Str, "one two\nthree", 'leading and trailing render newline-joined';
}

# A definedness smiley constrains the variable, not the `.=` invocant.
{
    my Int:D $n .= new;
    is $n, 0, 'my T:D $x .= new calls T.new, not "T:D".new';
}

# ---------------------------------------------------------------------------
# Metamodel::Trusting -- the `trusts` trait and .^trusts reflection
# ---------------------------------------------------------------------------

class Truster { trusts Int; trusts Str; }
is Truster.^trusts.map(*.^name).join(','), 'Int,Str',
   '.^trusts lists the trusted types in declaration order';

class NoTrust { }
is NoTrust.^trusts.elems, 0, '.^trusts of a class with no trusts is empty';
is Int.^trusts.elems, 0, '.^trusts of a builtin class is empty';

dies-ok { EVAL 'module TrustMod { }; TrustMod.^trusts' },
    '.^trusts is absent from a module metaclass';
dies-ok { EVAL 'enum TrustEnum <ta tb>; TrustEnum.^trusts' },
    '.^trusts is absent from an enum metaclass';

class TopCaller { ... }
class TopTrusted { trusts TopCaller; method !secret() { 'from TopTrusted' } }
class TopCaller { method poke() { TopTrusted.new()!TopTrusted::secret() } }
is TopCaller.poke, 'from TopTrusted', 'a top-level class honors its `trusts`';

# Trust is not blanket permission: an untrusted caller is still refused.
dies-ok {
    EVAL 'class Closed { method !secret() { 1 } };
          class Prier { method poke() { Closed.new()!Closed::secret() } };
          Prier.poke'
}, 'an untrusted caller cannot reach a private method';

done-testing;
