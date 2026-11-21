use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the terms inside several
# `{ ... }` blocks interpolated into one string keep their ranges when the
# string sits in a package body, where the view converts a copy of the code.

plan 3;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $class ~~ /Var|Call|Block/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[class S { method m($s) { say("c {$s.from}-{$s.to}") } }]),
    'Block@8-55 Blockoid@8-55 ParameterTarget::Var@19-21 VarDeclaration::Simple@19-21 Blockoid@23-53 Call::Name@25-51 Block@32-41 Blockoid@32-41 Var::Lexical@33-35 Call::Method@36-40 Block@42-49 Blockoid@42-49 Var::Lexical@43-45 Call::Method@46-48',
    'two blocks in a method';
is placed(Q[class T { sub m($s) { "{$s.a}{$s.b}" } }]),
    'Block@8-40 Blockoid@8-40 ParameterTarget::Var@16-18 VarDeclaration::Simple@16-18 Blockoid@20-38 Block@23-29 Blockoid@23-29 Var::Lexical@24-26 Call::Method@27-28 Block@29-35 Blockoid@29-35 Var::Lexical@30-32 Call::Method@33-34',
    'two adjacent blocks in a sub in a class';
is placed(Q[role R { method m($/) { "{$/.from} {$/.to}" } }]),
    'Blockoid@7-47 ParameterTarget::Var@18-20 VarDeclaration::Simple@18-20 Blockoid@22-45 Block@25-34 Blockoid@25-34 Var::Lexical@26-28 Call::Method@29-33 Block@35-42 Blockoid@35-42 Var::Lexical@36-38 Call::Method@39-41',
    'two blocks in a role method';
