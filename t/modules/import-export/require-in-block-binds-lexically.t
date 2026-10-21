# `require M <...>` inside a block binds the listed symbols in that block
# (ADR-8751, #14140): a closure taken there keeps them after the block left,
# and a routine declared outside the block never sees them. A `require`
# without a list loads the module and imports nothing.
use Test;
use lib 't/lib';

plan 11;

sub outside-call { (try EVAL 'rsi-f()') // 'not visible' }
sub outside-lookup { (try ::('&rsi-f')).defined }

my &c = do {
    require RequireScopedImports <&rsi-f $rsi-v RsiC RSI-K>;
    -> { (rsi-f(), $rsi-v, RsiC.hi, RSI-K).join(' ') }
};
is c(), 'rsi-f rsi-v rsi-c 42', 'a closure keeps the symbols its block required';

{
    require RequireScopedImports <&rsi-f>;
    is rsi-f(), 'rsi-f', 'the block calls the required routine';
    is outside-call(), 'not visible', 'a routine declared outside does not see it';
    nok outside-lookup(), 'nor finds it by indirect lookup';
}
is outside-call(), 'not visible', 'the import ends with the block';

{
    require RequireScopedImports;
    nok outside-lookup(), 'a require without a list imports no routine';
    is (try EVAL 'rsi-f()') // 'not visible', 'not visible',
        'nor into the scope holding it';
    is ::('RequireScopedImports').^name, 'RequireScopedImports',
        'but the module is loaded';
}

{
    my $name = 'RequireScopedImports';
    require ::($name) <&rsi-f $rsi-v>;
    is rsi-f() ~ ' ' ~ $rsi-v, 'rsi-f rsi-v', 'a computed module name binds the listed symbols too';
    nok outside-lookup(), 'without a routine alias outside the block';
}

throws-like { require RequireScopedImports <&not-there> },
    X::Import::MissingSymbols, 'an unexported routine is still missing';
