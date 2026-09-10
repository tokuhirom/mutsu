use Test;

plan 4;

# A class may override `Mu`'s default `.Capture` to control how it
# destructures against a sub-signature parameter (e.g. Cro::HTTP::Body's
# `MultiPartFormData does Associative`, whose `method Capture` builds the
# named-arg hash from its `@.parts` rather than reflecting its own public
# attributes). `Signature.ACCEPTS` and real block binding both used to
# destructure any Instance by reading its raw attributes directly, ignoring
# a user-defined `Capture` override — so a class like this never matched a
# destructuring sub-signature (`-> (:$name!, :$surname!) {...}`), the
# mechanism Cro::HTTP::Router's `request-body` handler uses.

class MultiPartLike does Associative {
    has %.parts;
    method AT-KEY(Str() $key) { %!parts.AT-KEY($key) }
    method EXISTS-KEY(Str() $key) { %!parts.EXISTS-KEY($key) }
    method Capture() { Capture.new(hash => %!parts) }
}

my $body = MultiPartLike.new(parts => { name => 'John', surname => 'Doe' });
my &handler = -> (:$name!, :$surname!) { "Hello, $name $surname!" };

ok &handler.signature.ACCEPTS(\($body)),
    "Signature.ACCEPTS coerces via a user-defined Capture override";
is &handler.($body), "Hello, John Doe!",
    "real binding also coerces via the user-defined Capture override";

# A class WITHOUT an explicit Capture override keeps the default behavior
# (destructure from its own public attributes) — this must not regress.
class PlainAttrs {
    has $.name = 'Ann';
    has $.surname = 'Lee';
}
my $plain = PlainAttrs.new;
ok &handler.signature.ACCEPTS(\($plain)),
    "default Capture (no override) still destructures from public attributes";
is &handler.($plain), "Hello, Ann Lee!",
    "default Capture binding still works for a class without an override";
