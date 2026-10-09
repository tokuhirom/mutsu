use experimental :rakuast;
use Test;

plan 11;

# Statement::Use and Statement::Require take the Rakudo constructor shape
# (`module-name => Name`), as Statement::Expression already did (#12446).
my $use = RakuAST::Statement::Use.new(
    module-name => RakuAST::Name.from-identifier("Test"));
is $use.^name, 'RakuAST::Statement::Use', 'Use.new builds a Use node';
is $use.module-name.^name, 'RakuAST::Name', 'Use module-name is the Name';
ok !$use.argument.defined, 'Use argument is absent when not given';
ok $use ~~ RakuAST::Statement, 'Use is a Statement';

my $req = RakuAST::Statement::Require.new(
    module-name => RakuAST::Name.from-identifier("Foo"));
is $req.^name, 'RakuAST::Statement::Require', 'Require.new builds a Require node';
is $req.module-name.^name, 'RakuAST::Name', 'Require module-name is the Name';
ok !$req.file.defined, 'Require file is absent when not given';
ok $req ~~ RakuAST::Statement, 'Require is a Statement';

dies-ok { RakuAST::Statement::Use.new }, 'Use.new without module-name dies';
dies-ok { RakuAST::Statement::Require.new }, 'Require.new without module-name dies';
dies-ok { RakuAST::Statement::Use.new(module-name => 42) },
    'Use.new rejects a non-Name module-name';
