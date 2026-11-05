use Test;
use lib 'roast/packages/Test-Helpers/lib';
use Test::Util;

plan 6;

# The program's final statement is sunk, a bare block ending the program
# included: a bare regex that ends that block matches the topic and runs its
# code blocks (#16679).
is_run '{ $_ = "x"; /{ say "ran" } x/; }',
    { out => "ran\n", err => '', status => 0 }, 'a regex ending the final block matches';
is_run '{ $_ = "x"; /{ say "B" } x/; }; say "next";',
    { out => "B\nnext\n", err => '', status => 0 }, 'the same block not last';
is_run '$_ = "x"; /{ say "top" } x/;',
    { out => "top\n", err => '', status => 0 }, 'a final statement regex outside a block';
is_run '{ say "a"; my $x = 5; }',
    { out => "a\n", err => '', status => 0 }, 'an ordinary final block still runs';
is_run '{ LEAVE say "left"; say "body"; }',
    { out => "body\nleft\n", err => '', status => 0 }, 'a final block with a LEAVE phaser';
is_run 'sub f { { $_ = "x"; /x/ } }; say f().^name',
    { out => "Regex\n", err => '', status => 0 }, 'a sub body tail keeps its value';
