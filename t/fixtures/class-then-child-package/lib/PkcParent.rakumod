use v6.d;
unit class PkcParent;
use PkcParent::Child;

has $.value = 42;
method how-name { $?CLASS.HOW.^name }
