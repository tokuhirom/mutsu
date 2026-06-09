use Test;

plan 7;

# A failing PRE/POST phaser throws X::Phaser::PrePost carrying the phaser name
# and the condition's source text, with a "Pre/Postcondition '<cond>' failed"
# message.

throws-like 'my sub a { PRE 0 }; a()', X::Phaser::PrePost,
    phaser => 'PRE', condition => /0/;

throws-like 'my sub a { POST 0 }; a()', X::Phaser::PrePost,
    phaser => 'POST', condition => /0/;

throws-like 'my sub a { PRE 0 }; a()', X::Phaser::PrePost,
    message => /:s Precondition .0. failed/;

throws-like 'my sub a { POST 0 }; a()', X::Phaser::PrePost,
    message => /:s Postcondition .0. failed/;

# A satisfied PRE/POST does not throw.
lives-ok { EVAL 'my sub a { PRE 1; 42 }; a()' }, 'satisfied PRE lives';
lives-ok { EVAL 'my sub a { POST 1; 42 }; a()' }, 'satisfied POST lives';

# Block-form PRE still enforces the condition (a different code path).
throws-like 'my sub a(Int $i) { PRE { $i > 0 }; 5 }; a(-1)', X::Phaser::PrePost,
    phaser => 'PRE';
