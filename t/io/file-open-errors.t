use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# File-system errors use Rakudo's wording (#9878): one shared helper module
# (src/runtime/native_io/fs_errors.rs) for every routine. Two families:
#
# - opening a file dies with an X::AdHoc reading
#   "Failed to open file <absolute path>: <strerror text>" (no "(os error N)"),
#   slurping a directory says "Tried to open directory <absolute path>", and
#   IO::Handle.open fails with X::IO::Directory naming the path as written;
# - the libuv-backed operations throw X::IO::* whose os-error is
#   "Failed to <op>: <uv_strerror text>" between the two absolute paths.
#
# Every message is measured against rakudo; paths are relative to an
# `indir` so the absolute form ($*CWD joined, not normalised) is visible.

plan 30;

my $dir = make-temp-dir;
my $abs = $dir.absolute;

sub message-of(&code) {
    code();
    CATCH { default { return .^name ~ ': ' ~ .message } }
    'no exception'
}

indir $dir, {
    mkdir 'adir';
    spurt 'file', 'hi';

    my $missing = "X::AdHoc: Failed to open file $abs/nope: No such file or directory";
    is message-of({ slurp 'nope' }), $missing, 'slurp sub';
    is message-of({ slurp 'nope', :bin }), $missing, 'slurp sub :bin';
    is message-of({ 'nope'.IO.slurp }), $missing, 'IO::Path.slurp';
    is message-of({ open('nope').sink }), $missing, 'open sub (sunk Failure)';
    is message-of({ 'nope'.IO.open.sink }), $missing, 'IO::Path.open';
    is message-of({ 'nope'.IO.lines }), $missing, 'IO::Path.lines';
    is message-of({ 'nope'.IO.words }), $missing, 'IO::Path.words';
    is message-of({ EVALFILE 'nope' }), $missing, 'EVALFILE';
    grammar G { token TOP { a } }
    is message-of({ G.parsefile('nope') }), $missing, 'Grammar.parsefile resolves against $*CWD';
    is message-of({ slurp '../nope' }),
        "X::AdHoc: Failed to open file $abs/../nope: No such file or directory",
        'the absolute path is not normalised';

    is message-of({ spurt('nodir/x', 'a').sink }),
        "X::AdHoc: Failed to open file $abs/nodir/x: No such file or directory", 'spurt sub';
    is message-of({ spurt('adir', 'a').sink }),
        "X::AdHoc: Failed to open file $abs/adir: Is a directory", 'spurt onto a directory';
    is message-of({ 'file'.IO.spurt('a', :createonly).sink }),
        "X::AdHoc: Failed to open file $abs/file: File exists", 'spurt :createonly';
    is slurp('file'), 'hi', '... and the existing file is untouched';

    is message-of({ slurp 'adir' }), "X::AdHoc: Tried to open directory $abs/adir",
        'slurp of a directory';
    is message-of({ EVALFILE 'adir' }), "X::AdHoc: Tried to open directory $abs/adir",
        'EVALFILE of a directory';
    is message-of({ open('adir').sink }),
        "X::IO::Directory: 'adir' is a directory, cannot do '.open' on a directory",
        'open of a directory fails with X::IO::Directory';
    is message-of({ 'adir'.IO.lines }),
        "X::IO::Directory: 'adir' is a directory, cannot do '.open' on a directory",
        'IO::Path.lines of a directory';
    is open('file').path.Str, 'file', 'an opened handle keeps the path as written';

    is message-of({ 'nope'.IO.d.sink }),
        "X::IO::DoesNotExist: Failed to find '$abs/nope' while trying to do '.d'",
        'a file test names the absolute path';

    is message-of({ copy('nope', 'b').sink }),
        "X::IO::Copy: Failed to copy '$abs/nope' to '$abs/b': Failed to copy file: no such file or directory",
        'copy of a missing file';
    is message-of({ copy('file', 'adir').sink }),
        "X::IO::Copy: Failed to copy '$abs/file' to '$abs/adir': Failed to copy file: illegal operation on a directory",
        'copy onto a directory uses the libuv wording';
    is message-of({ 'file'.IO.copy('file').sink }),
        "X::IO::Copy: Failed to copy '$abs/file' to '$abs/file': source and target are the same",
        'copy onto itself';
    {
        my $f = copy('nope', 'b');
        is-deeply ($f.exception.from, $f.exception.to, $f.exception.os-error),
            ("$abs/nope", "$abs/b", 'Failed to copy file: no such file or directory'),
            'X::IO::Copy carries from / to / os-error';
    }
    is message-of({ rename('nope', 'b').sink }),
        "X::IO::Rename: Failed to rename '$abs/nope' to '$abs/b': Failed to rename file: no such file or directory",
        'rename of a missing file';
    is message-of({ move('nope', 'b').sink }),
        "X::IO::Move: Failed to move '$abs/nope' to '$abs/b': Failed to copy file: no such file or directory",
        'move reports its copy step';
    ok rename('file', 'file'), 'renaming a file onto itself succeeds';

    is message-of({ 'nope'.IO.rmdir.sink }),
        "X::IO::Rmdir: Failed to remove the directory '$abs/nope': Failed to rmdir: no such file or directory",
        'IO::Path.rmdir';
    is message-of({ 'nope'.IO.chmod(0o755).sink }),
        "X::IO::Chmod: Failed to set the mode of '$abs/nope' to '0o755': Failed to set permissions on path: no such file or directory",
        'IO::Path.chmod';
    is message-of({ 'file'.IO.mkdir.sink }),
        "X::IO::Mkdir: Failed to create directory '$abs/file' with mode '0o777': Failed to mkdir: file already exists",
        'IO::Path.mkdir onto a file';
}
