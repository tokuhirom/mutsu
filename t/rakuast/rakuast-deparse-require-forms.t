use Test;
plan 12;
for 'my $p; require ::($p)', 'require "x.pm6"', 'my $f; require $f', 'require Foo <&bar>',
    'my $p; require ::($p) <&b>', 'require "x.pm6" <a b>' -> $src {
    is $src.AST.DEPARSE.chomp.subst(/\s+/, ' ', :g), $src.subst(/\s+/, ' ', :g), "deparse: $src";
}
my $file = Q[require "x.pm6"].AST.statements[0];
ok !$file.module-name.defined, 'file form: module-name is a type object';
is $file.module-name.^name, 'RakuAST::Name', 'file form: module-name type';
is $file.file.^name, 'RakuAST::QuotedString', 'file form: file';
my $dyn = Q[my $p; require ::($p)].AST.statements[1];
is $dyn.module-name.^name, 'RakuAST::Name', 'dynamic form: module-name';
ok !$dyn.file.defined, 'dynamic form: no file';
is EVAL(Q[my $p = "Test"; require ::($p); 42].AST), 42, 'EVAL of the AST still requires';
