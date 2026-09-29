use Test;

plan 13;

my $path = "tmp/io-path-stat-chown-{$*PID}.txt".IO;
$path.spurt('metadata');

ok $path.inode > 0, '.inode reads the filesystem inode';
isa-ok $path.dev, Int, '.dev reads the filesystem device';
isa-ok $path.devtype, Int, '.devtype is an integer';
is $path.inode, $path.inode, 'repeated inode reads agree';
ok $path.chown, '.chown with no owner change succeeds';
is $path.^can('chown').elems, 1, '.chown is visible to method introspection';

for <inode dev devtype> -> $method {
    my $missing = "tmp/io-path-stat-chown-missing-{$*PID}".IO;
    my $failure = $missing."$method"();
    isa-ok $failure, Failure, ".$method on a missing path fails";
    $failure.Bool;
}

my $missing = "tmp/io-path-stat-chown-missing-{$*PID}".IO;
my $chown-failure = $missing.chown;
isa-ok $chown-failure, Failure, '.chown on a missing path fails';
$chown-failure.Bool;
my $named-failure = $missing.chown(:uid(0));
isa-ok $named-failure, Failure, '.chown accepts a named uid';
$named-failure.Bool;
throws-like { $path.chmod($missing.mode) }, X::IO::DoesNotExist,
    '.chmod propagates a failed mode argument';
throws-like { (fail 'smartmatch failure') ~~ $path }, X::AdHoc,
    'smartmatching a Failure against IO::Path throws its exception';

$path.unlink;
