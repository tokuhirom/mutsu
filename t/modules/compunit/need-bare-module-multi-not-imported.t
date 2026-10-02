use Test;

# Neither a `need` statement nor `CompUnit::Repository.need` imports anything:
# a package-less module's exported proto/multi families must stay invisible
# to the loading scope (#11004). Checked against Rakudo.

plan 7;

use lib 't/lib/bare-multi-scope';
need BareMultiNeed;

throws-like { EVAL 'bmn-proto(1)' }, X::Undeclared::Symbols,
    'need does not import an exported proto-led family';
throws-like { EVAL 'bmn-multi(1)' }, X::Undeclared::Symbols,
    'need does not import an exported multi family';
throws-like { EVAL 'bmn-private(1)' }, X::Undeclared::Symbols,
    'nor an unexported one';

my $repo = CompUnit::Repository::FileSystem.new(:prefix<t/lib/bare-multi-scope>);
ok $repo.need(CompUnit::DependencySpecification.new(:short-name<BareMultiRepoNeed>)),
    'the repository loads the module';
throws-like { EVAL 'bmr-proto(1)' }, X::Undeclared::Symbols,
    'CompUnit::Repository.need does not import an exported proto-led family';
throws-like { EVAL 'bmr-multi(1)' }, X::Undeclared::Symbols,
    'nor an exported multi family';
throws-like { EVAL 'bmr-private(1)' }, X::Undeclared::Symbols,
    'nor an unexported one';
