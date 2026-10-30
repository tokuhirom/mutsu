use Test;

# `RakuAST::CompUnit.add-sorry` / `add-worry` queue a compile-time
# diagnostic, reported when the unit's compile phase ends: sorries are a
# `===SORRY!===` and exit code 1, worries "Potential difficulties" on stderr
# (#15666; `static`'s `fail-on-*` modes). Expected output is rakudo 2026.09's.

plan 20;

my $class = 'class E does X::Comp { has $.n; method message { "boom msg {$!n}" } };';

sub compile(Str $code) {
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    (exit => $proc.exitcode, out => $proc.out.slurp(:close), err => $proc.err.slurp(:close))
}

my %one = compile $class ~ ' CHECK { $*CU.add-sorry(E.new(:n(1))) }; say "ran"';
is %one<exit>, 1, 'a sorry fails the compilation';
is %one<out>, '', '... so the mainline never runs';
is %one<err>.lines.join("\n"),
    qq:to/END/.chomp,
    ===SORRY!=== Error while compiling -e
    boom msg 1
    at -e:1
    ------> <BOL><HERE>class E does X::Comp \{ has \$.n; method m
    END
    'a lone sorry is reported with its own heading, at the first token';

my %pos = compile $class
    ~ ' CHECK { my $e = E.new(:n(2)); $*CU.add-sorry($e); $e.SET_FILE_LINE("f.raku", 7); $e.SET_PRE_POST("x", "y") }';
is %pos<exit>, 1, 'a sorry whose position was reset afterwards still fails';
is %pos<err>.lines.join("\n"),
    "===SORRY!=== Error while compiling f.raku\nboom msg 2\nat f.raku:7\n------> x<HERE>y",
    '... and the position set after add-sorry wins';

my %two = compile $class ~ ' CHECK { $*CU.add-sorry(E.new(:n($_))) for 1, 2 }';
is %two<exit>, 1, 'two sorries fail the compilation';
is %two<err>.lines.join("\n"),
    ("===SORRY!===", |(1, 2).map({ ("boom msg $_", "at -e:1",
        '------> <BOL><HERE>class E does X::Comp { has $.n; method m') }).flat, '').join("\n"),
    'several sorries are bundled under one heading';

my %queued = compile $class ~ ' BEGIN $*CU.add-check-code({ $*CU.add-sorry(E.new(:n(3))) })';
is %queued<exit>, 1, 'a sorry added by queued check code fails the compilation';
like %queued<err>, /'boom msg 3'/, '... and is reported';

my %worry = compile $class ~ ' CHECK { $*CU.add-worry(E.new(:n(4))) }; say "ran"';
is %worry<exit>, 0, 'a worry does not fail the compilation';
is %worry<out>, "ran\n", '... the mainline runs';
is %worry<err>.lines.join("\n"),
    "Potential difficulties:\n    boom msg 4\n    at -e:1\n    ------> <BOL><HERE>class E does X::Comp \{ has \$.n; method m",
    '... and the worry is printed to stderr';

my %mixed = compile $class ~ ' CHECK { $*CU.add-sorry(E.new(:n(5))); $*CU.add-worry(E.new(:n(6))) }';
is %mixed<exit>, 1, 'a sorry with a worry fails the compilation';
like %mixed<err>, /^ '===SORRY!===' \n 'boom msg 5' .* 'Other potential difficulties:' \n '    boom msg 6'/,
    '... and reports the worries after the sorries';

ok RakuAST::CompUnit.^can('add-sorry'), 'RakuAST::CompUnit can add-sorry';
ok RakuAST::CompUnit.^can('add-worry'), 'RakuAST::CompUnit can add-worry';

# `$*CU.sorries` / `worries` list what was queued so far (static's t/07).
my %seen = compile $class ~ ' CHECK { my $cu = $*CU; note $cu.sorries.elems, $cu.worries.elems, $cu.has-check-time-problems; $cu.add-worry(E.new(:n(7))); note $cu.sorries.elems, $cu.worries.elems, $cu.has-check-time-problems; $cu.worries[0].n.note }';
like %seen<err>, /^ '00False' \n '01True' \n '7'/, 'sorries, worries and has-check-time-problems follow add-worry';
ok RakuAST::CompUnit.^can('sorries'), 'CompUnit can sorries';
ok RakuAST::CompUnit.^can('worries'), 'CompUnit can worries';
ok RakuAST::CompUnit.^can('has-check-time-problems'), 'CompUnit can has-check-time-problems';
