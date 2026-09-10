use Test;

plan 8;

# A `$^a`/`@^a`/`%^a` placeholder belongs to the innermost block that encloses
# it, so one written inside a *nested* block is that block's parameter and says
# nothing about whether the outer braces compose a Hash.

isa-ok { status => sub { 0 != $^a } }, Hash,
    'a placeholder inside a nested sub leaves the outer braces a Hash';
isa-ok { status => { $^a } }, Hash,
    'a placeholder inside a nested bare block leaves the outer braces a Hash';
isa-ok { a => 1, b => sub { $^x } }, Hash,
    'a placeholder in the last of several pairs still leaves a Hash';
isa-ok { a => { $^x }, b => { $^y } }, Hash,
    'placeholders in two nested blocks still leave a Hash';

# The hash keeps its pairs, and the nested block is a callable taking the
# placeholder as its own parameter.
my $composed = { status => sub { 0 != $^a } };
ok $composed<status>(1), 'the nested sub is callable and sees its own placeholder';
nok $composed<status>(0), 'the nested sub returns False for 0';

# A placeholder at the immediate level of the braces still forces a block.
ok { a => $^x } ~~ Callable,
    'a placeholder in a pair value at the immediate level composes a block';
ok { $^x } ~~ Callable,
    'a lone placeholder composes a block';
