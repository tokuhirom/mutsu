use Test;

plan 54;

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

# A `my class` nested in another class body must have its `trusts` honored.
# The nested class registers under a mangled lexical storage name
# (`Outer::Inner\0<decl-id>`, ADR-0047 P1), and by the time `Outer`'s method
# runs, the bare name `Inner` is no longer bound in the env the private-call
# permission check consults -- so the owner written in `$o!Inner::secret` has
# to be canonicalized against the invocant's own MRO instead.
class Outer {
    my class Inner {
        trusts Outer;
        method !secret() { 'from Inner' }
    }
    method poke() { Inner.new()!Inner::secret() }
}
is Outer.poke, 'from Inner', 'a nested lexical class honors its `trusts`';

class OuterOur {
    our class InnerOur {
        trusts OuterOur;
        method !secret() { 'from InnerOur' }
    }
    method poke() { InnerOur.new()!InnerOur::secret() }
}
is OuterOur.poke, 'from InnerOur', 'a nested `our` class honors its `trusts`';

# Trust is not blanket permission: without `trusts`, the same shape is refused.
dies-ok {
    EVAL 'class Nest {
              my class Shut { method !secret() { 1 } }
              method poke() { Shut.new()!Shut::secret() }
          };
          Nest.poke'
}, 'a nested lexical class without `trusts` still refuses an outer caller';

# ---------------------------------------------------------------------------
# Regression: a `trusts` declaration inside a nested class body must not
# package-qualify every OTHER class in the same compilation unit
# (todo/tickets/nested-trusts-decl-qualifies-sibling-class-names.md).
#
# Root cause: `Interpreter::run_class_body` sets the runtime's current
# package to the class being registered, walks the class body, then restores
# the saved package -- but an error from anywhere in that walk (e.g.
# `validate_private_access_in_stmts` rejecting a qualified private call to a
# nested class that has not registered yet, which happens when a hoisted
# forward-reference *shell* registration runs a `poke`-shaped method early)
# propagated straight past the restore via `?`. `exec_register_decl_op`
# swallows that error for a hoisted shell and keeps going, but the
# interpreter's runtime package stayed wrongly stuck on the half-registered
# class for the rest of the compilation unit, silently mis-qualifying every
# class registered afterward. This was not specific to `trusts` bodies --
# any class-body statement that can fail mid-walk had the same bug -- so the
# fix restores the package (and rolls back the env) unconditionally on every
# exit path out of the body walk, not just the successful one.
# ---------------------------------------------------------------------------

class NestedTrustPlain { }
is NestedTrustPlain.^name, 'NestedTrustPlain',
   'an unrelated sibling class keeps its bare, unqualified name';

class NestedTrustOuter {
    our class NestedTrustInner {
        trusts NestedTrustOuter;
        method !secret() { 'from NestedTrustInner' }
    }
    method poke() { NestedTrustInner.new()!NestedTrustInner::secret() }
}
is NestedTrustPlain.^name, 'NestedTrustPlain',
   'the sibling class name is still unqualified after a nested `trusts` class registers';
is NestedTrustOuter.poke, 'from NestedTrustInner',
   'the qualified private call into the nested `trusts` class still works';

# Dropping any one leg of the trigger (nesting, `trusts`, or the qualified
# call) never mis-qualified `Plain` -- but the general fix above stops the
# leak regardless, so this stays correct too.
class NestedNoTrustPlain { }
class NestedNoTrustOuter {
    our class NestedNoTrustInner {
        method !secret() { 'from NestedNoTrustInner' }
        method callit() { self!secret() }
    }
    method poke() { NestedNoTrustInner.new().callit() }
}
is NestedNoTrustPlain.^name, 'NestedNoTrustPlain',
   'no `trusts` + an unqualified private call: the sibling name stays unqualified';
is NestedNoTrustOuter.poke, 'from NestedNoTrustInner',
   'the unqualified private call inside the nested class still works';

# The mis-qualified name was the KEY other metadata is stored under, so it
# silently broke `:ver`/`:auth`/`:api` lookups on any class declared later in
# the same file as a nested `trusts` class.
class NestedTrustVerD:ver<1.2.3> { }
is NestedTrustVerD.^ver, v1.2.3,
   ':ver on a class declared after a nested `trusts` class is still readable';

done-testing;
