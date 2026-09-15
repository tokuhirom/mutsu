use Test;

# A pointy block handed to ^add_method receives the receiver as its first
# block parameter. That parameter must remain available in the method body;
# it is not an extra method argument.

plan 2;

class Added { }
Added.^add_method('identity', -> $receiver { $receiver.^name });
Added.^add_method('with-suffix', -> $receiver, $suffix { $receiver.^name ~ $suffix });
Added.^compose;

is Added.new.identity, 'Added',
    'the first pointy-block parameter aliases the method receiver';
is Added.new.with-suffix('!'), 'Added!',
    'later pointy-block parameters remain ordinary method arguments';
