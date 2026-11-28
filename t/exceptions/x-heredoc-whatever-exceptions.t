use Test;

# Exception classes Rakudo 2026.11 added to the setting (#19274):
# X::Syntax::Heredoc::* (heredoc body name checks, thrown by #19284) and
# X::Whatever::ShortCircuit (the `* && 1` worry).

plan 19;

nok X::Syntax::Heredoc.DEFINITE, 'X::Syntax::Heredoc resolves';
nok X::Whatever.DEFINITE, 'X::Whatever resolves';
is X::Whatever.gist, '(Whatever)', 'X::Whatever gist';

my \Amb = X::Syntax::Heredoc::AmbiguousName;
is Amb.^mro.map(*.^name).join(','), 'X::Syntax::Heredoc::AmbiguousName,Exception,Any,Mu',
    'AmbiguousName MRO';
ok Amb ~~ X::Syntax, 'AmbiguousName does X::Syntax';
ok Amb ~~ X::Comp, 'and so X::Comp';
is Amb.new(symbol => '$y').message,
    "Name '\$y' in a heredoc body is ambiguous: it means one declaration\n"
    ~ "where the heredoc starts and another where its body is written",
    'AmbiguousName message';

my \Hid = X::Syntax::Heredoc::HiddenName;
is Hid.^mro.map(*.^name).join(','),
    'X::Syntax::Heredoc::HiddenName,X::Undeclared,Exception,Any,Mu', 'HiddenName MRO';
ok Hid ~~ X::Undeclared, 'HiddenName isa X::Undeclared';
ok Hid ~~ X::Comp, 'HiddenName does X::Comp';
is Hid.new(symbol => '$x', what => 'Variable').message,
    "Name '\$x' in a heredoc body is declared only inside a block that closes\n"
    ~ "on the heredoc's line, before the body. Move the closing brace of the\n"
    ~ "block after the body to use it",
    'HiddenName message';
is Hid.new(symbol => 'foo', what => 'routine').message,
    "Name 'foo' in a heredoc body is declared only inside a block that\n"
    ~ "closes on the heredoc's line, before the body. Move the closing brace of\n"
    ~ "the block after the body to use it",
    'HiddenName message wraps by the symbol length';

my \SC = X::Whatever::ShortCircuit;
is SC.^mro.map(*.^name).join(','), 'X::Whatever::ShortCircuit,Any,Mu',
    'ShortCircuit is not an Exception';
nok SC ~~ Exception, 'not an Exception subtype';
my $sc = SC.new(operator => '&&', what => 'Whatever');
is $sc.operator, '&&', 'operator accessor';
is $sc.what, 'Whatever', 'what accessor';
is $sc.message,
    "Whatever tested first by && does not curry the && expression,\n"
    ~ "and is always true and defined.\n"
    ~ "Try a block using \$_ instead if results are not as expected.",
    'ShortCircuit message';

# The older non-exception worry class takes the same shape.
nok X::WhateverCode::SmartMatch::LHS ~~ Exception, 'X::WhateverCode::SmartMatch::LHS is not an Exception';
is X::WhateverCode::SmartMatch::LHS.new.message,
    "WhateverCode on LHS of smart-match does not curry the smart-match expression.\n"
    ~ "Try placing the WhateverCode expression on the RHS instead if results are not as expected.",
    'LHS message';
