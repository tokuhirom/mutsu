use v6;
use Test;

# IO::Path.raku renders its SPEC explicitly for a plain IO::Path.
like "foo/bar".IO.raku,
    /^ 'IO::Path.new("foo/bar", :SPEC(IO::Spec::Unix), :CWD(' /,
    'IO::Path.raku includes :SPEC(IO::Spec::Unix)';

# A bare current-directory path has `..` as its parent, with or without a
# trailing separator.
is "./".IO.parent.Str,  "..", "'./'.IO.parent is '..'";
is ".".IO.parent.Str,   "..", "'.'.IO.parent is '..'";
is "foo".IO.parent.Str, ".",  "'foo'.IO.parent is '.'";
is "/etc/foo".IO.parent.Str, "/etc", "'/etc/foo'.IO.parent is '/etc'";
is "/".IO.parent.Str,   "/",   "'/'.IO.parent is '/'";

# `unlink` as a no-paren listop accepts a word-quote argument, returns an Array
# of the paths it removed (a non-existent path counts as removed), and silently
# drops a path it could not remove (e.g. a directory).
my $dir = $*TMPDIR.add("mutsu-unlink-test-{$*PID}");
$dir.mkdir;
my $f = $dir.add("f.txt");
$f.spurt("x");
my $sub = $dir.add("subdir");
$sub.mkdir;

my @removed = unlink $f.Str, $sub.Str, $dir.add("nope.txt").Str;
is @removed.WHAT.^name, 'Array', 'unlink returns an Array';
is @removed.elems, 2, 'unlink drops the directory, keeps file + non-existent';
ok $f.Str (elem) @removed.Set, 'removed file is listed';
nok $f.e, 'the file was actually removed';

# The `.unlink` method fails softly (Failure) on a directory rather than throwing.
my $result = $sub.unlink;
ok $result ~~ Failure, '.unlink on a directory returns a Failure';
like $result.exception.message,
    /'Failed to remove the file' .* 'illegal operation on a directory'/,
    '.unlink failure message matches raku wording';

$sub.rmdir;
$dir.rmdir;

done-testing;
