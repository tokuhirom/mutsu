use Test;

# `X::Comp` carries a compile-time position and renders it as a `===SORRY!===`
# report (#15666; `static` composes it for its `fail-on-*` modes). Expected
# values are what rakudo 2026.09 answers.

plan 24;

class E does X::Comp { method message { "boom msg" } }

my $e = E.new;
is-deeply ($e.line, $e.filename, $e.column, $e.pos, $e.pre, $e.post, $e.directive-filename).map(*.defined).List,
    (False xx 7).List, 'a fresh exception has no position';
is $e.is-compile-time, False, 'it is not a compile-time exception yet';
is-deeply $e.highexpect, [], 'highexpect is empty';
is-deeply $e.modules, [], 'modules is empty';
is $e.sorry_heading, "===SORRY!=== Error while compiling:\n", 'heading without a file';
is $e.gist, 'boom msg', 'gist of an unpositioned exception is its message';

$e.SET_FILE_LINE('f.raku', 3);
is $e.filename, 'f.raku', 'SET_FILE_LINE sets the file name';
is $e.line, 3, '... and the line';
is $e.is-compile-time, True, '... and marks the exception compile-time';
is $e.sorry_heading, "===SORRY!=== Error while compiling f.raku\n", 'heading names the file';
is $e.gist, "===SORRY!=== Error while compiling f.raku\nboom msg\nat f.raku:3",
    'gist without an excerpt';

$e.SET_PRE_POST('aa', 'bb');
is-deeply ($e.pre, $e.post), ('aa', 'bb'), 'SET_PRE_POST sets the excerpt';
is $e.gist, "===SORRY!=== Error while compiling f.raku\nboom msg\nat f.raku:3\n------> aa<HERE>bb",
    'gist with an excerpt';
is $e.gist(:!sorry), "boom msg\nat f.raku:3\n------> aa<HERE>bb", 'gist(:!sorry) drops the heading';
is $e.Str, 'boom msg', 'Str is the message';

my $x = E.new(:highexpect<a b>, :modules[{:module<M1>, :filename<m1.rakumod>, :line(1)}, {:module<M2>, :filename<m2.rakumod>, :line(2)}]);
$x.SET_FILE_LINE('f.raku', 3);
$x.SET_PRE_POST('aa', 'bb');
is $x.gist, "===SORRY!=== Error while compiling f.raku\nboom msg\nat f.raku:3\n------> aa<HERE>bb"
    ~ "\n    expecting any of:\n        a\n        b\n  from module M1 (m1.rakumod line 1)",
    'gist lists the expectations and the module chain';
is $x.gist(:!expect), "===SORRY!=== Error while compiling f.raku\nboom msg\nat f.raku:3\n------> aa<HERE>bb"
    ~ "\n  from module M1 (m1.rakumod line 1)", 'gist(:!expect) drops the expectations';

is E.new.at-line(), 'at lines ', 'at-line without a file or lines';
is E.new.at-line(1), '', 'at-line for line 1 without a file';
is E.new.at-line(2), 'at line 2', 'at-line for one line without a file';
is E.new.at-line(1, 2), 'at lines 1, 2', 'at-line for several lines without a file';
is $e.at-line(1, 2), 'at f.raku:1,2', 'at-line with the exception file';
is $e.at-line(1, 2, :filename<z>), 'at z:1,2', 'at-line with :filename';

ok X::Comp::AdHoc.new(payload => 'x') ~~ X::Comp, 'a core X::Comp exception still matches the role';
