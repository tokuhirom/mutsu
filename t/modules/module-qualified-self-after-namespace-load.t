use Test;
use lib 't/lib';

plan 2;

use QualifiedSelfRoot;
use QualifiedSelfRoot::Child;

is qualified-self-child(), 'QualifiedSelfRoot::Child',
    'a module sees its own qualified class after the namespace root loads';
is QualifiedSelfRoot::Child.new.^name, 'QualifiedSelfRoot::Child',
    'the importing compunit also sees the qualified class';
