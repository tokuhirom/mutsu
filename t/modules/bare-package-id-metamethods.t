use Test;

plan 4;

# A bare `package` (as opposed to `class`/`module`) composes `PackageHOW`,
# which never gains `.^ver`/`.^auth`/`.^api` -- "ID metamethods on a package
# are absent by design" (roast S12-introspection/meta-class.t). mutsu already
# threw the right exception TYPE for this, but the message leaked the internal
# dispatcher's own wording ("Unknown method value dispatch (fallback
# disabled): auth") instead of a normal "No such method" -- and tacked on a
# nonsensical "Did you mean 'put'?" suggestion, since the candidate pool for
# an ordinary missing *method* has nothing to do with a missing meta-method
# (#9795).
package BarePackage9795:ver<1.2.3>:auth<me> { }

throws-like { BarePackage9795.^ver }, X::Method::NotFound,
    message => /'No such method \'ver\' for invocant of type \'BarePackage9795\''/,
    '.^ver on a bare package names the missing meta-method cleanly';

throws-like { BarePackage9795.^auth }, X::Method::NotFound,
    message => /'No such method \'auth\' for invocant of type \'BarePackage9795\''/,
    '.^auth on a bare package names the missing meta-method cleanly';

my $auth-message = try { BarePackage9795.^auth };
unlike $!.message, /'Did you mean'/,
    '.^auth on a bare package suggests nothing (it is not an ordinary method typo)';

# A `class`/`module` composes a HOW that does have these -- unaffected.
class ClassWithVer9795 { }
nok ClassWithVer9795.^ver.defined,
    'a class with no declared :ver answers an undefined type object, not an error';
