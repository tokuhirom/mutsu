use Test;

# `temp` / `let` over an operand that is not a container (#18623): the
# prefix, not a call to a sub named `temp`. The operand runs, then
# X::Localizer::NoContainer is thrown, as in Rakudo.

plan 11;

sub functionB { 'xxx' }
{
    try { temp &functionB.wrap({ 'yyy' }); };
    isa-ok $!, X::Localizer::NoContainer, 'temp over a wrap call throws';
    is $!.message, "Can only use 'temp' on a container", 'its message names temp';
    is functionB(), 'yyy', 'the operand ran before the throw';
}

throws-like { temp 5 }, X::Localizer::NoContainer, localizer => 'temp',
    'temp over a literal';
throws-like { let 5 }, X::Localizer::NoContainer, localizer => 'let',
    message => "Can only use 'let' on a container", 'let over a literal';
throws-like { temp &functionB }, X::Localizer::NoContainer, 'temp over a code variable';
throws-like { temp 5 = 3 }, X::Localizer::NoContainer, 'temp over a literal with an assignment';

my $calls = 0;
sub counted { $calls++ }
try { temp counted() + 1 };
is $calls, 1, 'temp binds tighter than +: its operand is the call';

is X::Localizer::NoContainer.new(localizer => 'temp').message,
    "Can only use 'temp' on a container", 'a hand-built exception has the message';

my $x = 1;
{ temp $x = 2; is $x, 2, 'temp over a variable still saves it' }
is $x, 1, 'and restores it';
