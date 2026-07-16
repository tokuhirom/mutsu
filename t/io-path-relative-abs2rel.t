use Test;

# IO::Path.relative($base) computes the path relative to $base the way raku's
# $*SPEC.abs2rel does: make both absolute, drop the common prefix, and prepend a
# `..` for each remaining base component. A previous mutsu bug only handled the
# case where $base is a literal ancestor (plain strip-prefix) and otherwise
# returned the *absolute* path unchanged. That corrupted zef's install pipeline,
# whose extract phase builds a `tar -C` target from `$archive.relative($tmp)`.

plan 12;

# sibling base -> walk up then down
is "/tmp/A/x".IO.relative("/tmp/B"),     "../A/x",      'sibling base walks up with ..';
# ancestor base -> plain descent
is "/tmp/A/B/x".IO.relative("/tmp/A"),   "B/x",         'ancestor base descends';
# equal paths -> "."
is "/tmp/A/x".IO.relative("/tmp/A/x"),   ".",           'equal path is "."';
# target is an ancestor of base -> all ..
is "/tmp/A".IO.relative("/tmp/A/B/C"),   "../..",       'target above base is all ..';
# only the root is common
is "/a/b/c".IO.relative("/x/y"),         "../../a/b/c", 'only root common';
# trailing slash on base is ignored
is "/tmp/A/x".IO.relative("/tmp/A/"),    "x",           'trailing slash on base ignored';

# relative receiver and/or relative base resolve against $*CWD first
{
    my $*CWD = "/base/dir".IO;
    is "sub/f".IO.relative,               "sub/f",       'relative receiver, default base = $*CWD';
    is "/base/dir/sub/f".IO.relative,     "sub/f",       'absolute receiver under $*CWD';
    is "/base/x".IO.relative,             "../x",        'non-descendant walks up';
    is "sub/f".IO.relative("/base"),      "dir/sub/f",   'relative receiver, absolute base';
    is "/base/dir/a".IO.relative("dir2"), "../a",        'both resolved against $*CWD';
}

# `.` segments drop; `..` stays literal and is compared verbatim (raku abs2rel).
is "/tmp/A/./x".IO.relative("/tmp/B/../A"), "../../../A/x", 'dot drops, dotdot literal';
