use Test;
use lib $?FILE.IO.parent(2).add('lib');
use NativeCstructImport::User;
plan 3;

# Loading the module must not warn "Not an accepted NativeCall type":
# an imported `my class ... is repr('CStruct')` is a valid NativeCall type.
my $lib = $?FILE.IO.parent(2).add('lib');
my $proc = run $*EXECUTABLE, '-I', $lib.Str, '-e', 'use NativeCstructImport::User; say "loaded"', :out, :err;
my $err = $proc.err.slurp(:close);
is $err, '', 'no NativeCall type warning for an imported CStruct parameter' or diag $err;

my @reprs = NativeCstructImport::User::reprs();
is @reprs.join(','), 'CStruct,CStruct,CStruct',
    'the parameter types of native and plain subs report the CStruct repr';

# The imported class is the one the parameter names.
is NativeCstructImport::CProb.REPR, 'CStruct', 'and so does the class itself';
