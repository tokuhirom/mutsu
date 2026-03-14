use Test;

plan 4;

{
    dies-ok { EVAL q[role A { method m { 1 } }; role B { method m { 2 } }; class C does A does B { }] },
        'conflicting role methods die';
    try {
        EVAL q[role A { method m { 1 } }; role B { method m { 2 } }; class C does A does B { }];
        CATCH {
            default {
                ok .message.contains("Method 'm' must be resolved by class C"),
                    'error mentions unresolved role method conflict';
            }
        }
    }
}

lives-ok {
    EVAL q[
        role A { method m { 1 } }
        role B { method m { 2 } }
        class C does A does B { method m { 3 } }
        is C.new.m, 3, 'class method resolves conflicting role methods';
    ];
}, 'class-defined method resolves role conflict';
