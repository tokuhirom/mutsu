use Test;

# Code.name, Code.line and Code.file are rows of the method table
# (ADR-12523, slice 4).

plan 14;

sub f(Int $a) { }
my &g = -> $x { };
multi mm(Int $a) { }
multi mm(Str $a) { }
class C { method m($x) { } }
my $here = $?FILE.IO.basename;

is &f.name, 'f', 'a declared sub';
is &g.name, '', 'an anonymous pointy block';
is (sub ($a) { }).name, '', 'an anonymous sub';
is &mm.name, 'mm', 'a multi dispatcher';
is C.^lookup('m').name, 'm', 'a method object';
is &infix:<+>.name, 'infix:<+>', 'an operator';
is /a/.name, '', 'an anonymous regex';
my $re = /a/; $re.set_name('zz');
is $re.name, 'zz', 'set_name is seen by the row';

is &f.line, 8, 'line of a declared sub';
is &f.file.IO.basename, $here, 'file of a declared sub';
is &g.line, 9, 'line of a pointy block';
is C.^lookup('m').line, 12, 'line of a method';
is (sub ($a) { }).line, 29, 'line of an anonymous sub';
is { $_ }.file.IO.basename, $here, 'file of a bare block';
