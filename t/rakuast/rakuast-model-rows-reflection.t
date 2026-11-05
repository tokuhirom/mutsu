use Test;
use experimental :rakuast;

# #16805: a method the RakuAST model answers through a native row is reported
# by `.^can`, `.can`, `.^find_method` and `.^lookup`, on the class and on an
# instance, including the ones a role declares (`WHY` of
# `Doc::DeclaratorTarget`, `has-placeholder-parameters` of
# `PlaceholderParameterOwner`). It was callable but invisible.

plan 25;

my $sub = RakuAST::Sub.new(name => RakuAST::Name.from-identifier('f'));

ok $sub.^can('WHY').so, 'an instance can WHY (a role method)';
ok $sub.^can('set-WHY').so, 'an instance can set-WHY';
ok $sub.^can('replace-body').so, 'an instance can replace-body';
ok $sub.can('replace-body').so, '.can finds it';
ok $sub.^find_method('replace-body').defined, '.^find_method finds it';
ok $sub.^lookup('replace-body').defined, '.^lookup finds it';
ok $sub.^can('has-placeholder-parameters').so, 'has-placeholder-parameters (PlaceholderParameterOwner)';
ok $sub.^can('name').so, 'a field accessor is still found';
nok $sub.^can('no-such-method-here').so, 'an unknown name is not';

for <Sub Method Submethod PointyBlock Block Class Parameter Type::Subset Routine> -> $c {
    my $t = ::("RakuAST::$c");
    ok $t.^can('WHY').so, "$c can WHY";
}

for <Sub Method Submethod PointyBlock Block> -> $c {
    my $t = ::("RakuAST::$c");
    ok $t.^can('replace-body').so, "$c can replace-body";
}
# only the classes that do PlaceholderParameterOwner have it
nok RakuAST::Class.^can('has-placeholder-parameters').so, 'Class does not have has-placeholder-parameters';
nok RakuAST::Parameter.^can('replace-body').so, 'Parameter does not have replace-body';
