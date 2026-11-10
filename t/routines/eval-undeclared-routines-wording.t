use Test;

plan 8;

sub msg(Str $code) { try { EVAL $code }; $!.message }

is msg('foo'), "Undeclared routine:\n    foo used at line 1", 'one bareword is a routine';
like msg('foo; bar'), /'Undeclared routines:'/, 'two barewords: plural wording';
like msg('foo; bar'), /bar .* foo/, 'listed sorted by name';
like msg('floobody doop'), /'Undeclared routines:'/, 'a call and a bareword: plural';
like msg('floobody doop'), /doop/, 'the bareword is listed';
like msg('floobody doop'), /floobody/, 'the call is listed';
is (try { EVAL 'foo; bar'; CATCH { default { .^name } } }), 'X::Undeclared::Symbols', 'type';
is msg('floobody 1'), "Undeclared routine:\n    floobody used at line 1", 'a single call stays singular';

# vim: expandtab shiftwidth=4
