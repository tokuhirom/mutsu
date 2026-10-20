use Test;
use MONKEY-SEE-NO-EVAL;
plan 33;

my @invalid =
    'my class A { proto method a(|) { {*} } method b {2} }',
    'my class A { proto method a(|) {*} method b {2} }',
    'my class A { proto method a(|) {} method b {2} }',
    'my class A { my proto method a(|) {*} method b {2} }',
    'my class A { our proto method a(|) {*} method b {2} }',
    'proto sub f(|) {*} sub g {2}',
    'proto sub f(|) {} sub g {2}',
    'my proto sub f(|) {*} sub g {2}',
    'my grammar G { proto token a {*} token b {b} }',
    'my grammar G { proto rule a {*} rule b {b} }',
    'my grammar G { proto regex a {*} regex b {b} }',
    'proto sub outer(|) { proto sub inner(|) {*} sub after {2} }',
    'sub f {} sub g {2}',
    'my class A { proto method a(|) {*} #`(inline) method b {2} }',
    "my class A \{ proto method a(|) \{*} #`(multi\nline) method b \{2} }",
    'my class A { proto method a(|) {*} ' ~ 160.chr ~ 'method b {2} }';
for @invalid -> $source {
    throws-like { EVAL $source }, X::Syntax::Confused,
        message => / 'Strange text after block' /,
        'a block needs a separator before another declaration';
}

my @valid =
    "my class P1 \{ proto method a(|) \{*}\nmethod b \{2} }",
    'my class P2 { proto method a(|) {*}; method b {2} }',
    'my class P3 { proto method a(|) {*}, method b {2} }',
    "my class P4 \{ proto method a(|) \{*} # comment\nmethod b \{2} }",
    'my class P5 { proto method a(|) {*} }',
    'proto sub end-of-input(|) {*}',
    'my class P6 { proto method a(|) {*} .gist; }',
    'proto sub proto-postfix(|) {*} .gist;',
    'my grammar P7 { proto token a {*} .gist; }',
    'sub ordinary-postfix {1} .gist;';
for @valid -> $source {
    lives-ok { EVAL $source }, 'a separator or routine postfix remains valid';
}

for 13, 11, 12, 133, 8232, 8233 -> $codepoint {
    my $name = 'Vertical' ~ $codepoint;
    my $source = 'my class ' ~ $name ~ ' { proto method a(|) {*} '
        ~ $codepoint.chr ~ 'method b {2} }; ' ~ $name ~ '.b';
    is EVAL($source), 2, "Unicode vertical whitespace $codepoint ends the declaration";
}
lives-ok { EVAL('proto sub horizontal-end(|) {*}' ~ 160.chr) },
    'horizontal Unicode whitespace before end-of-input remains valid';
