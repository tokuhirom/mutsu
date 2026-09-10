use v6.e.PREVIEW;
use Test;

plan 2;

grammar Foo {
    token TOP { \d+ }
}

is Foo.^mro.first(*.^name eq 'Grammar').^name, 'Grammar', 'grammar MRO includes Grammar';
is Grammar.^ver, '6.e', 'Grammar metaobject version is 6.e';
