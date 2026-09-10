use Test;

plan 2;

throws-like { EVAL q[class A { }; class A { }] }, X::Redeclaration,
    'duplicate class declarations in EVAL throw X::Redeclaration';
lives-ok { EVAL q[class G { ... }; class G { }] },
    'stub class can be redeclared by a real class in EVAL';
