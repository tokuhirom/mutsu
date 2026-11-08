use Test;
use experimental :rakuast;

plan 6;

# `my %h is MixHash` writes only the trait: the hash's Mix type is not a
# `my MixHash %h` type in the tree (#14845).
for 'my %h is MixHash', 'my %h is Mix', 'my %h is MixHash = a => 1' -> $src {
    is $src.AST.DEPARSE.trim, $src, "DEPARSE of '$src'";
}
ok !'my %h is MixHash'.AST.statements[0].expression.type.defined, 'no type node';
is 'my MixHash %h'.AST.DEPARSE.trim, 'my MixHash %h', 'a written type stays';
my %h is MixHash = a => 2;
is %h<a>, 2, 'the declaration still makes a MixHash';
