use v6;
use Test;

# ForwardIterables (ecosystem): `--> NAME` where NAME is a constant (a
# declared `constant`, or the core term IterationEnd) is a definite return
# value, like `--> 42`; it is not a type constraint named NAME.
plan 6;

constant K = 42;
sub a(--> K) { 1 }
is a(), 42, 'sub returns a declared constant';

sub b(--> IterationEnd) { 1 }
is b().raku, 'IterationEnd', 'sub returns IterationEnd';

class C {
    method m(\t --> IterationEnd) { 7 }
    method n(--> K) { 1 }
}
is C.new.m(1).raku, 'IterationEnd', 'method returns IterationEnd';
is C.new.n, 42, 'method returns a declared constant';

subset S of Int;
sub g(--> S) { 3 }
is g(), 3, 'a subset in --> position is still a type constraint';
sub h(--> Int) { 4 }
is h(), 4, 'a type in --> position is still a type constraint';
