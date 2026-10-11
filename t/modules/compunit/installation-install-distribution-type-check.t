use Test;

# `CompUnit::Repository::Installation.install` and `.uninstall` take
# `Distribution $distribution`: a value that is not one fails the binding
# instead of silently succeeding (#9887). Only a temporary repository is used.

plan 6;

my $tmp = $*TMPDIR.child("mutsu-inst-type-{$*PID}");
$tmp.mkdir;
my $repo = CompUnit::RepositoryRegistry.repository-for-spec("inst#{$tmp.absolute}");

throws-like { $repo.uninstall(Any) }, X::TypeCheck::Binding::Parameter,
    'uninstall(Any) fails the Distribution type check';
throws-like { $repo.install(Any) }, X::TypeCheck::Binding::Parameter,
    'install(Any) fails the Distribution type check';
throws-like { $repo.install("not a dist") }, X::TypeCheck::Binding::Parameter,
    'install of a Str fails it too';

try $repo.uninstall(Any);
like $!.message, /'parameter \'$distribution\'; expected Distribution but got Any'/,
    'the message names the parameter and the types';

class NotADist { }
throws-like { $repo.uninstall(NotADist.new) }, X::TypeCheck::Binding::Parameter,
    'an object that does not do Distribution is refused';

nok $tmp.child('dist').e, 'nothing was written to the repository';

END { try { $tmp.child('dist').rmdir if $tmp.child('dist').e; $tmp.rmdir if $tmp.e } }
