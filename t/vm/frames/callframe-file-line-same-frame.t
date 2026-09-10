use Test;
use lib 't/lib';
use FileVarFixture;

# `push_caller_env` used to build each `CallFrameEntry` from two independent
# sources: `.file` from `executing_source_file()` (walks the routine stack)
# and `.line` from the VM's global `cur_source_line` tracker. Those normally
# agree, but a method frame never carried a `def_file` at all
# (`push_method_routine_with_location` hardcoded `None`), so
# `executing_source_file()` fell through a method frame straight to the
# dynamically-scoped `?FILE` -- which had already reverted to the *calling*
# script by the time the method ran. `.line`, driven by the bytecode line
# table rather than the frame walk, still correctly pointed at the method's
# own position -- so `callframe(N).file` and `callframe(N).line` ended up
# describing two different files: `.file` named the caller's script, `.line`
# was a line number that only makes sense inside the module.
#
# This is exactly the shape rakudo's Test.rakumod hits when it walks
# `callframe` frames to attribute a failing assertion to the test script
# (see todo/tickets/callframe-line-and-file-come-from-different-frames.md):
# a failure got reported at the test file's name but a line number that
# belonged to Test.rakumod itself.

plan 4;

my $module = 't/lib/FileVarFixture.rakumod';
my $script = 't/callframe-file-line-same-frame.t';

my $cf = FixtureMethodProbe.new.probe;

ok $cf.file.contains($module),
    'callframe(0).file inside a module method reports the module, not the caller';
nok $cf.file.contains($script),
    'and specifically not the script that called the method';

# `method probe` is pinned to line 46 of FileVarFixture.rakumod (see the
# comment there) -- `callframe(0)` is called on that exact line, so `.line`
# must agree.
is $cf.line, 46, 'callframe(0).line agrees with the SAME frame .file names';

# Belt and braces: file and line must both come from the module, not one from
# each side of the call boundary.
ok $cf.file.contains($module) && $cf.line == 46,
    '.file and .line describe the same actual source position';
