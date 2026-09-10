use v6;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

# The X::Comp::Group raised when an undeclared bareword gobbles a required block
# (`when X::Undeclared { ... }`) used to reach the user as the bare text
# "X::Comp::Group: Missing block" — no offending name, no line. It builds the
# detailed raku-style wording internally, so surface that, and carry a position
# so the CLI can report the line. This is what makes the failure findable in a
# large file: Raku::Pod::Render's 1300-line ProcessedPod.rakumod fails this way
# (its `when X::LibCurl { … }` is undeclared because the LibCurl::Easy dependency
# is absent) and mutsu pointed nowhere.
#
# The exception *structure* is pinned by t/comp-group-when-gobbled.t; this file
# pins the human-facing text and location only.

plan 4;

my $prog = qq:to/END/;
say "line 1";
say "line 2";
try \{
    die "x";
    CATCH \{
        when X::NotDeclaredAnywhere \{ say 1 }
        default \{ say "d" }
    }
}
END

is_run $prog, { status => { $_ != 0 }, err => /'X::NotDeclaredAnywhere'/ },
    'the error names the offending bareword';

is_run $prog, { status => { $_ != 0 }, err => /'needs parens to avoid gobbling block'/ },
    'and uses the raku wording';

is_run $prog, { status => { $_ != 0 }, err => /'Missing block (apparently claimed by'/ },
    'and still reports the missing block';

# Line 6 is the `when` line; raku reports the same line for this program.
is_run $prog, { status => { $_ != 0 }, err => /':6'/ },
    'and points at the offending line';
