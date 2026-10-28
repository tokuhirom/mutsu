class LexicalClassTrusts { ... }

my class LexicalClassTrusts::Item {
    trusts LexicalClassTrusts;
    has Str $.name = 'item';
    method !secret(Str:D $prefix --> Str) { $prefix ~ $!name }
}

class LexicalClassTrusts:ver<0.1.0>:auth<zef:test> {
    method reveal { LexicalClassTrusts::Item.new!LexicalClassTrusts::Item::secret('x-') }
}
