use Test;

# #15853: `.VAR` of a native-typed lexical is a native reference, and `.VAR.of`
# of a List item that carries a typed variable's cell is that variable's type.

plan 10;

{
    my int $i = 1; my uint8 $u = 2; my byte $b = 3;
    my num $n = 1e0; my str $s = "a";
    is $i.VAR.^name, 'IntLexRef', 'int';
    is $u.VAR.^name, 'UIntLexRef', 'uint8';
    is $b.VAR.^name, 'UIntLexRef', 'byte';
    is $n.VAR.^name, 'NumLexRef', 'num';
    is $s.VAR.^name, 'StrLexRef', 'str';
    is ($b,)[0].VAR.^name, 'UIntLexRef', 'a List item holding the native cell';
}

{
    my Int $t = 1;
    my Int $plain = 2;
    is $plain.VAR.^name, 'Scalar', 'a non-native typed variable is still a Scalar';
    is ($t,)[0].VAR.of.raku, 'Int', 'the List item reports the cell type';
    my $u = 3;
    is ($u,)[0].VAR.of.raku, 'Mu', 'an unconstrained cell is Mu';
    is $t.VAR.of.raku, 'Int', 'and the variable itself still does';
}
