unit module RoleProtoImports::Helper;

sub admit(Capture:D $c --> Str) is export(:internals) { "admitted " ~ $c.elems }
sub plain-helper(--> Str) is export { 'plain' }
