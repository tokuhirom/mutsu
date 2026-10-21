use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

my $g = EVAL(Q[
    grammar SourceSpellings {
        token tight { a|b }
        token before { a |b }
        token after { a| b }
        token both { a | b }
        token seq-tight { a||b }
        token seq-before { a ||b }
        token seq-after { a|| b }
        token seq-both { a || b }
        token quoted { 'a' | "b" }
        token leading { | a | b }
        token leading-seq { || a || b }
        token triple { a ||| b }
        token mixed { a | b || c }
        regex rx { "a" | 'b' }
        token parameterized( Int $x = 1 ) { $x }
        rule spaced { a | b }
    }
].AST);

for (
    'tight' => 'a|b', 'before' => 'a |b', 'after' => 'a| b', 'both' => 'a | b',
    'seq-tight' => 'a||b', 'seq-before' => 'a ||b',
    'seq-after' => 'a|| b', 'seq-both' => 'a || b',
    'quoted' => Q['a' | "b"],
    'leading' => '| a | b', 'leading-seq' => '|| a || b',
    'triple' => 'a ||| b', 'mixed' => 'a | b || c',
) -> $case {
    is $g.^lookup($case.key).gist.trim,
        'token ' ~ $case.key ~ ' { ' ~ $case.value ~ ' }',
        'declaration source preserves ' ~ $case.key;
    ok $g.parse('a', :rule($case.key)), 'first branch matches for ' ~ $case.key;
    ok $g.parse('b', :rule($case.key)), 'second branch matches for ' ~ $case.key;
}
is $g.^lookup('spaced').gist.trim, 'rule spaced { a | b }', 'rule source retains alternation trivia';
ok $g.parse('a ', :rule<spaced>), 'rule sigspace consumes trailing whitespace';
is $g.^lookup('rx').gist.trim, Q[regex rx { "a" | 'b' }], 'a regex declaration retains quote delimiters';
ok $g.parse('b', :rule<rx>), 'the regex declaration still executes';
is $g.^lookup('parameterized').gist.trim, 'token parameterized( Int $x = 1 ) { $x }',
    'a signature retains its written whitespace';
is $g.parse('1', :rule<parameterized>).Str, '1', 'a signature default still binds';

my $mutable = Q[grammar MutatedSource { token a($n,$m="") { $n $m }; token TOP { <a("a")> }}].AST;
my $top = $mutable.statements[0].expression.body.body.statement-list.statements.tail.expression;
$top.body.regex.args.push(RakuAST::QuotedString.new(segments => (RakuAST::StrLiteral.new('b'),)));
my $changed = EVAL($mutable);
is $changed.parse('ab').Str, 'ab', 'a public ArgList mutation changes regex execution';
is $changed.^lookup('TOP').gist, 'token TOP { <a("a")> }',
    'declaration reflection retains its original source after an AST edit';

my $program = Q[use RakuASTRegexSourceFixture; say RakuASTRegexSourceFixture::Cached.^lookup('TOP').gist.trim; say RakuASTRegexSourceFixture::Cached.parse('b').Str; my $anon = RakuASTRegexSourceFixture::anonymous(); say $anon.gist.trim; say ('b' ~~ $anon).Str];
my @outputs;
my @statuses;
for ^2 {
    my $proc = run 'env', 'MUTSU_RAKUAST=all', $*EXECUTABLE,
        '-I', 't/lib', '-e', $program, :out, :err;
    @outputs.push: $proc.out.slurp(:close).trim;
    my $err = $proc.err.slurp(:close);
    @statuses.push: $proc.exitcode;
}
is @outputs[0], Q[token TOP { 'a' | "b" }] ~ "\nb\n" ~ Q[token { 'a' | "b" }] ~ "\nb",
    'a cold module keeps named and anonymous declaration source and behavior';
is @outputs[1], @outputs[0], 'a warm module keeps the same declaration source and behavior';
is @statuses.join(','), '0,0', 'both module executions exit successfully';

my $comment-grammar = EVAL("grammar CommentSource \{ token TOP \{ a # don't normalize | here\n | b } }".AST);
is $comment-grammar.^lookup('TOP').gist.trim,
    "token TOP \{ a # don't normalize | here\n | b }", 'comments retain their original declaration text';
is $comment-grammar.parse('b').Str, 'b', 'a comment before the separator does not become a branch';

my $tab-grammar = EVAL("grammar TabSource \{ token TOP \{ a\t|\tb } }".AST);
is $tab-grammar.^lookup('TOP').gist.trim, "token TOP \{ a\t|\tb }", 'separator tabs remain tabs';
is $tab-grammar.parse('b').Str, 'b', 'tab trivia does not change matching';

is EVAL(Q[
    my $ran = 0;
    my sub don't { $ran++ }
    'a' ~~ / a { don't() } /;
    $ran
].AST), 1, 'an identifier apostrophe does not open a string in regex code';

is EVAL(Q[
    my $ran = 0;
    'a' ~~ / a { $ran = “}”.chars } /;
    $ran
].AST), 1, 'a brace in a Unicode code quote does not close the code block';

is EVAL(Q[
    my $ran = 0;
    'a' ~~ / a { #`( } don't " )
        $ran++;
    } /;
    $ran
].AST), 1, 'an embedded comment keeps its braces and quotes out of code scanning';

my $direct = token { a| b };
is $direct.gist.trim, 'token { a| b }', 'an anonymous token retains its declaration source';
is $direct.raku.trim, 'token { a| b }', 'an anonymous token raku uses declaration provenance';
is ('b' ~~ $direct).Str, 'b', 'source reflection does not change an anonymous token match';

my $anonymous = EVAL(Q[regex { a || b }].AST);
is $anonymous.gist.trim, 'regex { a || b }', 'an anonymous regex retains its source across EVAL';
is $anonymous.raku.trim, 'regex { a || b }', 'an anonymous regex raku retains the source';
is ('b' ~~ $anonymous).Str, 'b', 'the anonymous regex still executes its lowered tree';

my $word = 'b';
my $captured = rule { $word | 'a' };
is $captured.gist.trim, Q[rule { $word | 'a' }], 'capturing a lexical keeps anonymous rule source';
is ('b' ~~ $captured).Str, 'b', 'the source metadata keeps the lexical capture live';

my $parameterized = EVAL(Q[token ( $x ) { $x | 'a' }].AST);
is $parameterized.gist.trim, Q[token ( $x ) { $x | 'a' }], 'an anonymous signature keeps its source';
is ('b' ~~ / <&$parameterized('b')> /).Str, 'b', 'signature binding keeps the declaration metadata';

done-testing;
