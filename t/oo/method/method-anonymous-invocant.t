use Test;

plan 3;

my method typed(Int:D: $arg) { self.^name ~ ' ' ~ $arg }
is 1.&typed('a'), 'Int a', 'a typed anonymous invocant binds one positional';

my method named($self: $arg) { $self.Str ~ ' ' ~ $arg }
is 1.&named('b'), '1 b', 'an explicit $self invocant is retained';

my method other(Int $receiver: $arg) { $receiver.Str ~ ' ' ~ $arg }
is 1.&other('c'), '1 c', 'a named typed invocant still binds normally';
