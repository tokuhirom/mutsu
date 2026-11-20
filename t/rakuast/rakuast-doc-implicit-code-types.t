use experimental :rakuast;
use Test;

# Pod blocks as RakuAST (#14880), measured on rakudo 2026.11: an indented
# paragraph is an `implicit-code` block only in `cell`/`defn`/`item`/
# `nested`/`pod`/`rakudoc`/`section`; elsewhere it is text as written. After
# a nested block, a paragraph indented like that block's first paragraph is
# text (rakudo's `!interpret-implicit-code-blocks`). A verbatim block ends at
# an unindented `=end` only.

plan 6;

sub shape($p) {
    $p ~~ RakuAST::Doc::Block
        ?? $p.type ~ '[' ~ $p.paragraphs.map(&shape).join('|') ~ ']'
        !! $p.Str.raku
}
sub doc(Str $src) { $src.AST.statements.grep(RakuAST::Doc::Block).map(&shape).join(' ') }

is doc("=begin Foo\n    y\n      z\n=end Foo\n"), Q<Foo["    y\n      z\n"]>,
    'a custom block keeps an indented paragraph as text';
is doc("=for Podcast\n    this is not\n\n"), Q<Podcast["    this is not\n\n"]>,
    'an indented =for body is text';
is doc("=begin head1\n    y\n=end head1\n"), Q<head["    y\n"]>,
    'a heading is no implicit-code type';
is doc("=begin pod\n    code\n\n    =for Podcast\n        not code\n\n    also text\n\n    =head1 Mumble\n\n    but code\n\n=end pod\n"),
    Q<pod[implicit-code["code\n\n"]|Podcast["    not code\n\n"]|"also text\n\n"|head["Mumble\n\n"]|implicit-code["but code\n\n"]]>,
    'after a nested block, its leading whitespace decides text or code';
is doc("=begin pod\n    =begin item\n        and this is!\n    =end item\n=end pod\n"),
    Q<pod[item[implicit-code["and this is!\n"]]]>,
    'an implicit code block inside an indented block';
is doc("=begin code\n    foo\n    =begin code\n    =end code\n=end code\n"),
    Q<code["    foo\n    =begin code\n    =end code\n"]>,
    'an indented =end inside a code block is text';
