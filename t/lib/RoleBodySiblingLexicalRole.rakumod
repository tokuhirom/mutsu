unit module RoleBodySiblingLexicalRole;
my role Ser { }
my class Str1 does Ser { method enc { 'str' } }
my proto mt(|) { * }
multi mt(Ser $t) { $t }
multi mt(Str:U) { Str1 }
my role Mapping[Any:U $k, Any:U $v] does Ser {
	my Ser:U $key-type = mt($k);
	my Ser:U $value-type = mt($v);
	method enc { $key-type.enc ~ $value-type.enc }
}
multi mt(Hash:U $h) { Mapping[$h.keyof, $h.of] }
our class Start {
	method go { my $t = mt(Hash[Str, Str]); $t.enc }
}
