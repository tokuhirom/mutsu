use Test;

plan 7;

# Only a routine that declares a return type is a `Callable[T]`; a plain
# `sub {}` is not a `Callable[Callable]` (or `Callable[Mu]`) (#16461).
throws-like { my Callable &c = sub { 2 } }, X::TypeCheck::Assignment,
    'my Callable &c rejects a routine without a declared return type';

lives-ok { my Callable &d = sub (--> Callable) { sub { 1 } } },
    'a routine declared --> Callable is accepted';
my Callable &e = sub (--> Callable) { sub { 1 } };
is e()(), 1, 'and it runs';

ok (sub (--> Callable) {}) ~~ Callable[Callable], 'declared return type matches Callable[Callable]';
nok (sub {}) ~~ Callable[Callable], 'a plain sub is not Callable[Callable]';
nok (sub {}) ~~ Callable[Mu], 'a plain sub is not Callable[Mu]';
throws-like { my Callable[Int] $y = sub { 1 } }, X::TypeCheck::Assignment,
    'my Callable[Int] $y rejects a routine without a declared return type';
