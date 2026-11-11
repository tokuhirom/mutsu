use Test;

# An untyped `:&x` named parameter is implicitly `Callable`, as its
# positional counterpart `&x` is. `f(e => { })` passes an empty Hash (`{ }`
# is a Hash literal), which used to bind silently. Found while measuring
# Terminal::ANSIParser's `make-ansi-parser(emit-item => ...)`.

plan 7;

sub f(:&e) { &e.^name }
sub g(:&e!) { &e.^name }

throws-like { f(e => { }) }, X::TypeCheck::Binding::Parameter,
    'an empty Hash does not bind to :&e';
throws-like { g(e => 42) }, X::TypeCheck::Binding::Parameter,
    'an Int does not bind to a required :&e!';
throws-like { f(e => %(a => 1)) }, X::TypeCheck::Binding::Parameter,
    'a Hash does not bind to :&e';
is f(e => -> $x { }), 'Block', 'a pointy block binds';
is f(e => &say), 'Sub', 'a Sub binds';
is f(), 'Callable', 'an omitted optional :&e is the Callable type object';
like (try f(e => 42)) // $!.message, /'expected Callable but got Int'/,
    'the message names Callable';
