use Test;

plan 6;

is $~MAIN, 'MAIN', '$~MAIN stays defined';
is $~Quote, 'Quote', '$~Quote stays defined';
is $~Regex, 'Regex', '$~Regex stays defined';

try EVAL q{$~Foo};
ok $!.defined, '$~Foo with an unknown slang throws';
like $!.message, /"No grammar is known for slang 'Foo'"/, 'message names the slang';

try EVAL q{$~P5Regex};
like $!.message, /"slang 'P5Regex'"/, '$~P5Regex (removed slang) is rejected';
