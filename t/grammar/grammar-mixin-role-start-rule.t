use Test;

plan 4;

role Rules {
    token foo { <bar> }
    token bar { 'x' }
}
grammar StaticGrammar does Rules { }
grammar DynamicGrammar { }
grammar InstanceGrammar { }

is StaticGrammar.subparse('x', :rule<foo>).Str, 'x',
    'a role composed in the grammar declaration supplies the start rule';
is DynamicGrammar.^mixin(Rules).subparse('x', :rule<foo>).Str, 'x',
    'a role mixed into the grammar type supplies the start rule and subrule';
is (InstanceGrammar.new but Rules).subparse('x', :rule<foo>).Str, 'x',
    'a role mixed into an instance supplies the start rule and subrule';
is InstanceGrammar.^mixin(Rules).parse('x', :rule<foo>).Str, 'x',
    'parse uses the role-mixed grammar class too';
