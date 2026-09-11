use v6;
use Test;

# A proto-regex candidate's `:sym<...>` adverb holds arbitrary text, `::`
# included: `rule pseudo:sym<::element>` is how CSS::Grammar::CSS3 spells the
# CSS3 pseudo-element selector, and the matching action method is
# `method pseudo:sym<::element>($/)`. Splitting such a method name on `::`
# invented a package qualifier `pseudo:sym<` and a method `element>`, so the
# action never dispatched ("Cannot dispatch to method element> on pseudo:sym<").

plan 4;

grammar G {
    proto token pseudo {*}
    token pseudo:sym<::element> { '::' $<element>=[<[\w-]>+] }
    token pseudo:sym<:element>  { ':'  $<element>=[<[\w-]>+] }
}

class Acts {
    method pseudo:sym<::element>($/) { make 'dcolon:' ~ $<element> }
    method pseudo:sym<:element>($/)  { make 'colon:'  ~ $<element> }
}

is G.parse('::my-elem', :rule<pseudo>, :actions(Acts.new)).ast, 'dcolon:my-elem',
    'a `::`-bearing sym dispatches its action method';
is G.parse(':my-elem', :rule<pseudo>, :actions(Acts.new)).ast, 'colon:my-elem',
    'the single-colon sibling still dispatches';

ok Acts.^can('pseudo:sym<::element>'), 'the method is registered under its full name';

# A genuine package qualifier outside the adverb still splits.
class Parent { method m { 'parent' } }
class Child is Parent { method m { 'child' } }
is Child.new.Parent::m, 'parent', 'an ordinary qualified call is unaffected';
