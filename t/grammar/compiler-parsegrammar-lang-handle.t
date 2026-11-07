use Test;
use nqp;

# Found through Jupyter::Kernel 1.0.5 (Handler.update-compiler):
# `$compiler.parsegrammar($lang.slang_grammar('MAIN'))` with `$lang = $?LANG`.
# HLL::Compiler's parsegrammar/parseactions are getter/setters.

plan 5;

my $compiler = nqp::getcomp('Raku');
my Mu $lang = $?LANG;
ok $lang.defined, '$?LANG is a defined language handle';
my Mu $grammar = $lang.slang_grammar('MAIN');
my Mu $actions = $lang.slang_actions('MAIN');
ok $grammar.defined || $grammar.WHAT !=== Mu, 'slang_grammar answers a handle';
ok $actions.defined || $actions.WHAT !=== Mu, 'slang_actions answers a handle';


$compiler.parsegrammar($grammar);
$compiler.parseactions($actions);
ok $compiler.parsegrammar === $grammar, 'parsegrammar answers what was set';
ok $compiler.parseactions === $actions, 'parseactions answers what was set';

