use Test;

plan 8;

sub msg(Str $code) { try { EVAL $code }; $!.message }

like msg('foo'), /^ 'Undeclared routine:' \s+ 'foo used at line 1' \s* $/, 'one bareword is a routine';
like msg('foo; bar'), /'Undeclared routines:'/, 'two barewords: plural wording';
like msg('foo; bar'), /bar .* foo/, 'listed sorted by name';
like msg('floobody doop'), /'Undeclared routines:'/, 'a call and a bareword: plural';
like msg('floobody doop'), /doop/, 'the bareword is listed';
like msg('floobody doop'), /floobody/, 'the call is listed';
try { EVAL 'foo; bar' }
is $!.^name, 'X::Undeclared::Symbols', 'type';
like msg('floobody 1'), /^ 'Undeclared routine:' \s+ 'floobody used at line 1' \s* $/, 'a single call stays singular';

# vim: expandtab shiftwidth=4
