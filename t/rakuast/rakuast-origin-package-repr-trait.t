use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a package whose header
# sets its representation (`is repr('CStruct')`, which the package absorbs)
# starts its block at its first trait, and the traits after the
# representation are placed.

plan 4;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q<<class T1 is repr("CStruct") { has int32 $.x }>>),
    'StatementList@0-45 Statement::Expression@0-45 Class@0-45 Name@6-8 Block@9-45 Blockoid@28-45 StatementList@30-43 Statement::Expression@30-43 VarDeclaration::Simple@30-43 Type::Simple@34-39 Name@34-39',
    'a representation alone';
is placed(Q<<class T2 is repr("P6opaque") is Int { }>>),
    'StatementList@0-39 Statement::Expression@0-39 Class@0-39 Name@6-8 Block@9-39 Trait::Is@29-35 Type::Simple@32-35 Name@32-35 Blockoid@36-39 StatementList@37-37',
    'a representation before a parent';
is placed(Q<<class T3 is repr('P6opaque') does Positional { }>>),
    'StatementList@0-48 Statement::Expression@0-48 Class@0-48 Name@6-8 Block@9-48 Trait::Does@29-44 Type::Simple@34-44 Name@34-44 Blockoid@45-48 StatementList@46-46',
    'a representation before a role';
is placed(Q<<class T4 is rw is repr("P6opaque") { }>>),
    'StatementList@0-38 Statement::Expression@0-38 Class@0-38 Name@6-8 Block@9-38 Trait::Is@9-14 Name@12-14 Blockoid@35-38 StatementList@36-36',
    'a representation after a flag';
