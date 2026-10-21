use Test;

plan 8;

# Assigning to a non-rw accessor names the attribute's value, as Rakudo does
# (#14266, AttrX::Mooish's `.message` check).
class C { has $.baz = 'is baz'; has Int $.n = 5; has $.u; }

for ('baz', X::Assignment::RO, 'Cannot modify an immutable Str (is baz)'),
    ('n',   X::Assignment::RO, 'Cannot modify an immutable Int (5)'),
    ('u',   X::Assignment::RO, "Cannot modify an immutable 'Any' type object") -> ($m, $type, $message) {
    throws-like { C.new."$m"() = 'x' }, $type, :message($message), "accessor $m";
}
throws-like { C.new.baz = 'x' }, X::Assignment::RO, :message('Cannot modify an immutable Str (is baz)'),
    'plain method-call assignment';
try { C.new.n = 1; CATCH { default { is .^name, 'X::Assignment::RO', 'exception type' } } }
try { C.new.n = 1; CATCH { default { is .typename, 'Int', 'typename attribute' } } }
try { C.new.u = 1; CATCH { default { like .message, /'type object'/, 'unset attribute names a type object' } } }
try { C.new.baz = 1; CATCH { default { ok .value ~~ Str, 'value attribute holds the value' } } }
