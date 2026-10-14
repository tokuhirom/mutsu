use Test;

plan 7;

throws-like 'EVAL q[class A { method a { 1 } method b { 2 } }]',
    X::Syntax::Confused,
    'a second method needs a separator after a same-line method body';

throws-like 'EVAL q[class B { multi method a { 1 } method b { 2 } }]',
    X::Syntax::Confused,
    'a multi method body also requires a separator';

throws-like 'EVAL q[class C { submethod a { 1 } submethod b { 2 } }]',
    X::Syntax::Confused,
    'a submethod body also requires a separator';

lives-ok { EVAL 'class D { method a { 1 }; method b { 2 } }' },
    'a semicolon separates same-line method declarations';

lives-ok {
    EVAL q:to/CODE/;
        class E { method a { 1 }
        method b { 2 } }
        CODE
},
    'a newline separates method declarations';

lives-ok {
    EVAL q:to/CODE/;
        class F { my submethod a { 1 }
        method b { 2 } }
        CODE
},
    'a lexical submethod followed by a newline still ends its statement';

lives-ok { EVAL 'class G { method m { 1 }.list }' },
    'a same-line postfix may follow a method declaration body';
