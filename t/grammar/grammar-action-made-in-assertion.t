use Test;

# A `<?{ ... }>` code assertion that references `$<x>.made` must see the action
# result of the just-matched named capture *during* parsing. raku runs grammar
# actions incrementally at reduce time; mutsu otherwise only ran them after the
# whole parse, leaving `.made` undefined inside assertions. This is what lets
# Template::Mustache's standalone-line rule
#   token linetag { ^^ (\h*) <tag> <?{ $<tag>.made<type> ~~ none(<var ...>) }> ... }
# distinguish a standalone section/partial line (whose surrounding whitespace is
# consumed) from an interpolation line (whose whitespace is preserved).

plan 7;

# --- 1. basic: `.made` of a named capture is available in an assertion ---
grammar G1 {
    token TOP { <thing> }
    token thing { <word> <?{ $<word>.made eq 'HELLO' }> }
    token word { (\w+) }
}
class A1 {
    method TOP($/)  { make 'ok' }
    method word($/) { make $0.uc }
}
ok G1.parse('hello', :actions(A1.new)), 'assertion passes when $<word>.made matches';
nok G1.parse('world', :actions(A1.new)), 'assertion fails when $<word>.made differs';

# --- 2. proto-regex variant dispatch: `.made<type>` selects the right action ---
grammar G2 {
    token TOP { <item>+ }
    token item { [ <line> | <tag> ] }
    token line { <tag> <?{ $<tag>.made<type> ne 'skip' }> '!' }
    proto token tag {*}
    token tag:sym<keep> { 'k' (\w) }
    token tag:sym<skip> { 's' (\w) }
}
class A2 {
    method TOP($/)  { make $<item>».made.join(',') }
    method item($/) { make $<line> ?? "LINE" !! "TAG:{$<tag>.made<type>}" }
    method line($/) { make 'line' }
    method tag:sym<keep>($/) { make { type => 'keep' } }
    method tag:sym<skip>($/) { make { type => 'skip' } }
}
# 'ka!' -> line (keep, assertion true, '!' consumed); 'sb' -> plain tag (skip rejected by line)
is G2.parse('ka!sb', :actions(A2.new)).made, 'LINE,TAG:skip',
    'proto-regex .made<type> drives the assertion per variant';

# --- 3. the standalone-line shape from Template::Mustache (regex + backtracking) ---
grammar G3 {
    regex TOP { ^ <hunk>* (.*) $ }
    regex hunk { (.*?) [ <linetag> | <tag> ] }
    token linetag { ^^ (\h*) <tag> <?{ $<tag>.made<type> ~~ none(<var>) }> \h* [\n|$] }
    proto regex tag {*}
    token tag:sym<var>     { '{{' \h* (\w+) \h* '}}' }
    regex tag:sym<section> { '{{' ('#'|'/') \h* (\w+) \h* '}}' }
}
class A3 {
    method TOP($/) {
        my @p;
        for $<hunk> -> $h { @p.push: $h.made }
        @p.push("LIT[{~$0}]") if $0.chars;
        make @p.join('');
    }
    method hunk($/) {
        my @x;
        @x.push("LIT[{~$0}]") if $0.chars;
        @x.push("LINETAG[{$<linetag>.made<type>}]") if $<linetag>.defined;
        @x.push("TAG[{$<tag>.made<type>}]") if $<tag>.defined;
        make @x.join('');
    }
    method linetag($/) { make $<tag>.made }
    method tag:sym<var>($/)     { make { type => 'var' } }
    method tag:sym<section>($/) { make { type => 'section' } }
}
# A standalone section close `{{/a}}\n` is matched by <linetag> (consuming the
# newline); a standalone var line is NOT (var excluded), so its newline leaks as
# a literal. This is the exact distinction Template::Mustache relies on.
is G3.parse("\{\{#a}}\{\{line}}\n\{\{/a}}\n", :actions(A3.new)).made,
    "TAG[section]TAG[var]LIT[\n]LINETAG[section]",
    'standalone section line consumed via linetag, var line preserved';

# --- 4. assertion sees nested/positional sub-captures of the made value ---
grammar G4 {
    token TOP { <pair> }
    token pair { <kv> <?{ $<kv>.made<v> > 10 }> }
    token kv { (\w) '=' (\d+) }
}
class A4 {
    method TOP($/) { make 'ok' }
    method kv($/)  { make { k => ~$0, v => +$1 } }
}
ok  G4.parse('a=42', :actions(A4.new)), 'assertion reads a numeric .made field (>10)';
nok G4.parse('a=5',  :actions(A4.new)), 'assertion rejects when .made field <= 10';

# --- 5. without :actions, .made-bearing assertion degrades gracefully (no crash) ---
# `$<word>.made` is Nil with no actions; `Nil eq 'HELLO'` is False -> no match.
nok G1.parse('hello'), 'no :actions -> .made is Nil -> assertion false (no crash)';
