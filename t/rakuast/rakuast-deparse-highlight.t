use Test;
use experimental :rakuast;
use nqp;
use RakuAST::Deparse::Highlight;

plan 18;

my $src = q:to/END/;
# a comment
my %h = a => 1, b => 2;   # trailing
sub foo(Int $x, :$y = 3 --> Str) {
    return "v: $x" if $x > 1;
}
say foo(2) ~ "z";
END

# The three roles rakudo ships, with the comments the actions collected.
is highlight($src, "DEBUG"), q:to/END/, 'DEBUG role: comments kept, code normalized';
# a comment
my %h = a => 1, b => 2;   # trailing
sub foo (Int $x, :$y = 3 --> Str) {
    return "v: $x" if $x > 1
}
say foo(2) ~ "z";
END
like highlight($src, "Text"), /^ "\e[34m# a comment\e[0m\n"/, 'Text role colours the leading comment';
like highlight($src, "HTML"), /'<span style="color:blue;">   # trailing</span>'/,
    'HTML role wraps the trailing comment';

# The safe actions refuse what could run code at compile time.
for 'BEGIN { 1 }' => 'BEGIN phaser',
    'constant X = 3;' => 'constant definition',
    'use Foo;' => 'module loading',
    'use nqp;' => 'use nqp' -> (:key($code), :value($what)) {
    my $r = highlight($code, "DEBUG");
    ok $r ~~ Failure, "'$code' is refused";
    is $r.exception.message, "$what not allowed in safe syntax highlighting", '... with the reason';
    $r.so;
}
is highlight('use strict;', "DEBUG"), "use strict;\n", 'a pragma is allowed';

# The actions object hears of the productions the source contains.
my constant RakuActions = nqp::gethllsym('Raku', 'Actions');
my constant RakuGrammar = nqp::gethllsym('Raku', 'Grammar');
ok RakuActions.^name eq 'Raku::Actions' && RakuGrammar.^name eq 'Raku::Grammar',
    'the Raku HLL symbols are the base classes';
my @seen;
class Spy is RakuActions {
    method comment:sym<#>(Mu $/) { @seen.push("comment {$/.from}-{$/.to}"); nextsame }
    method statement-control:sym<use>(Mu $/) { @seen.push("use {$/.from}-{$/.to}"); nextsame }
    method statement-prefix:sym<BEGIN>(Mu $/) { @seen.push("BEGIN {$/.from}-{$/.to}"); nextsame }
    method type-declarator:sym<constant>(Mu $/) { @seen.push("constant {$/.from}-{$/.to}"); nextsame }
    method doc-block:sym<finish>(Mu $/) { @seen.push("finish {$/.from}-{$/.to}"); nextsame }
}
"# one\nuse Test; #| doc\nmy \$x = 1; #= trail\nBEGIN \{ 1 }; #`(emb) constant Y = 2;\nmy \$s = 'a # not'; say 1; # last\n=finish\nhello # not\n".AST(:compunit, :actions(nqp::create(Spy)));
is @seen.unique.sort.join(', '), 'BEGIN 43-54, comment 0-5, comment 106-112, constant 64-78, finish 113-133, use 6-14', 'productions with their ranges';

is RakuAST::Pragma.IS-PRAGMA("strict"), 1, 'a known pragma';
is RakuAST::Pragma.IS-PRAGMA("Test"), 0, 'a module is no pragma';
is RakuAST::Pragma.IS-ISM("Perl5"), 1, 'a known -ism';
is "say 1;\n=finish\nhello\n".AST(:compunit).finish-content, "hello\n", 'CompUnit.finish-content';
