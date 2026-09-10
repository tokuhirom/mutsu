use Test;

plan 11;

# Signature.returns yields the declared return-type type object.
sub with-ret(--> Str) { "x" }
is &with-ret.signature.returns.^name, 'Str', 'sub --> Str: returns is Str';
ok &with-ret.signature.returns === Str, 'returns is the Str type object';

sub int-ret(Int $x --> Int) { $x }
is &int-ret.signature.returns.^name, 'Int', 'sub --> Int: returns is Int';

# No explicit return type defaults to Mu.
sub no-ret($x) { $x }
is &no-ret.signature.returns.^name, 'Mu', 'no return type: returns is Mu';
ok &no-ret.signature.returns === Mu, 'default returns is the Mu type object';

# Pointy block with a return type.
my &p = -> Int $x --> Str { "$x" };
is &p.signature.returns.^name, 'Str', 'pointy block --> Str: returns is Str';

# .returns is also reachable straight off a Signature literal value.
is (-> --> Int {}).signature.returns.^name, 'Int', 'anon block --> Int';

# A method signature.
class C { method m(--> Bool) { True } }
is C.^lookup("m").signature.returns.^name, 'Bool', 'method --> Bool: returns is Bool';

# .returns type object gist.
is &no-ret.signature.returns.gist, '(Mu)', 'Mu type object gist';
is &with-ret.signature.returns.gist, '(Str)', 'Str type object gist';

# The signature value is a Signature.
ok (-> --> Int {}).signature.^name eq 'Signature', 'signature value is a Signature';
