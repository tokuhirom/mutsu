use v6;
use Test;

# A `multi` candidate whose parameter default is a non-constant expression
# reading an earlier parameter must not emit a warning while the candidate is
# being *matched*. mutsu evaluated every unsupplied parameter's default on the
# dispatch path -- in a scope where the earlier parameters are not bound yet --
# even for a candidate with no `where` clause, which is the only consumer of
# that value. `$d = $c ~ "!"` therefore saw an unbound `$c`, warned "Use of Nil
# in string context", and threw the result away; the value finally bound was
# correct, so only the warning was wrong (#8078).
#
# The warning has to be observed on a subprocess's STDERR rather than with a
# `CONTROL`/`CX::Warn` handler: it was raised on the dispatch path, outside the
# caller's handler scope, so an in-process handler counted zero while the text
# still reached the terminal. Checking STDERR is what actually pins the bug.

plan 8;

sub run-snippet($code) {
    my $proc = run($*EXECUTABLE, '-e', $code, :out, :err);
    (
        out => $proc.out.slurp(:close).trim,
        err => $proc.err.slurp(:close),
    );
}

# --- the reported repro: two candidates, both with sibling-reading defaults ---
my $two = q:to/CODE/;
multi m(Int $c, $d = $c * 2)   { "int:$c/$d" }
multi m(Str $c, $d = $c ~ "!") { "str:$c/$d" }
say m("a");
say m(3);
CODE

my %two = run-snippet($two);
is %two<out>, "str:a/a!\nint:3/6", 'each candidate binds its own sibling-reading default';
is %two<err>, '', 'dispatching to either candidate prints nothing on STDERR';

# --- one candidate is enough: the losing candidate is not the cause ---
my %one = run-snippet(q:to/CODE/);
multi one(Str $c, $d = $c ~ "!") { "$c/$d" }
say one("z");
CODE
is %one<out>, 'z/z!', 'a lone multi candidate binds its sibling-reading default';
is %one<err>, '', 'a lone multi candidate prints nothing on STDERR';

# --- the plain-sub baseline, which was always silent ---
my %plain = run-snippet(q:to/CODE/);
sub plain(Str $c, $d = $c ~ "!") { "$c/$d" }
say plain("y");
CODE
is %plain<out>, 'y/y!', 'a plain sub binds its sibling-reading default';
is %plain<err>, '', 'a plain sub prints nothing on STDERR';

# --- a `where` clause DOES still consume the evaluated default (#8089) ---
# Dispatch has to agree with binding here: the first candidate below is
# selectable only because its omitted parameter's default is evaluated and
# tested against the `where`. Keeping that working is what stops the fix above
# from becoming "never evaluate a default during matching".
multi pick($c, $d where { $_ eq 'yes' } = 'yes') { "yes:$c" }
multi pick($c, $d)                               { "other:$c/$d" }
is pick(1),       'yes:1',      'a candidate is still selected on its defaulted where-parameter';
is pick(1, 'no'), 'other:1/no', 'a supplied argument still fails the where and picks the other candidate';
