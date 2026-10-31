use NativeCall;
use NativeCstructImport::Prob;
unit module NativeCstructImport::User;

my sub native-one(NativeCstructImport::CProb --> int32) is native('c') { * }
sub native-two(NativeCstructImport::CProb, NativeCstructImport::CProb) is native('c') { * }
my sub plain-one(NativeCstructImport::CProb) { }

our sub param-repr(&routine) { &routine.signature.params[0].type.REPR }
our sub reprs { (param-repr(&native-one), param-repr(&native-two), param-repr(&plain-one)) }
