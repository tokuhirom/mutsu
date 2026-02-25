use Test;

plan 6;

role WithStub { method a() { ... } }
role ProvidesStub1 { method a() { 1 } }
role ProvidesStub2 { method a() { 2 } }

dies-ok { EVAL 'class A does WithStub { }' }, 'class must implement required role stub';
dies-ok { EVAL 'class B does WithStub does ProvidesStub1 does ProvidesStub2 { }' },
    'ambiguous role implementations for a stubbed method die';

eval-lives-ok q[class C does WithStub { multi method a() { "ok" } }],
    'eval-lives-ok inherits outer declarations for role composition checks';

lives-ok { EVAL 'class D does WithStub { has WithStub $.with-stub handles <a> }' },
    'has handles composes delegated method for stub requirements';

{
    my role WithPrivate { method !foo { "p" } }
    my role WithPrivateStub { method !foo { ... } }
    my class PrivateUser does WithPrivate does WithPrivateStub { method bar { self!foo } }
    is PrivateUser.new.bar(), 'p', 'private method calls on instances without attributes work';
}

lives-ok {
    my role R { method m() { ... } }
    R.DUMP();
    R.WHERE();
    R.Numeric();
    R.Real();
}, 'role type object fallback methods do not execute stub code';
