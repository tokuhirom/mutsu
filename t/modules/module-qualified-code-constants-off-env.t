use Test;
use lib 't/lib';

# A module's top-level `constant &name` is published as the package symbol
# `&Pkg::name` (ADR-12529 phase 1). It lives in the package-symbol table, not
# in the importer's frame env; every way of reaching it must still find it.

plan 11;

sub load-in-sub() {
    require FrameEnvCodeConstants;
}

load-in-sub();
use FrameEnvCodeConstants :ALL;

is alt('a', 'b'), 'a|b', 'the exported short name calls the constant';
is tagged(1), 'tag:1', 'an exported pointy-block constant is callable';
is &FrameEnvCodeConstants::alt('c', 'd'), 'c|d', 'the qualified code variable is callable';
is FrameEnvCodeConstants::alt('e'), 'e', 'a qualified call finds the constant';
is ::('&FrameEnvCodeConstants::alt')('f'), 'f', 'indirect lookup finds the constant';
ok &FrameEnvCodeConstants::tagged ~~ Callable, 'the qualified code variable is a Callable';
is FrameEnvCodeConstants::.<&alt>('g'), 'g', 'the package stash exposes the constant';
is FrameEnvCodeConstants::own-alt('h', 'i'), 'h|i', 'the module calls its own constant';
is FrameEnvCodeConstants::make-closure()('j'), 'j|z', 'a closure in the module calls the constant';
is FrameEnvCodeConstants::own-qualified(), 'tag:q', 'the module reads its own qualified name';
is (await start { FrameEnvCodeConstants::alt('k', 'l') }), 'k|l', 'another thread finds the constant';
