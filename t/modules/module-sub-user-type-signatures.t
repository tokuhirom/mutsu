use Test;
use lib 't/lib';
use ModuleSubUserType;

# Regression distilled from Pinterest::URL::Normalizer 0.1.0.
plan 3;

my $result = make-result('ok');
isa-ok $result, Result, 'a module sub returns its package-local user type';
is result-value($result), 'ok', 'a module sub accepts its package-local user type';
is make-result('again').^name, 'ModuleSubUserType::Result',
    'the returned instance keeps its fully-qualified class identity';
