use Test;
plan 6;

# `is repr(Name)` where `Name` is a constant holding the representation's
# name (LibXML::Raw::Defs: `constant Opaque is export(:Opaque) = 'CPointer'`,
# used as `class xmlDict is repr(Opaque)`).
my constant Opaque = 'CPointer';
class A is repr(Opaque) { }
is A.REPR, 'CPointer', 'repr(CONSTANT) resolves the constant';

class B is repr('CPointer') { }
is B.REPR, 'CPointer', 'quoted repr unchanged';

class C { }
is C.REPR, 'P6opaque', 'default repr unchanged';

constant CS = 'CStruct';
class D is repr(CS) { has int32 $.x; }
is D.REPR, 'CStruct', 'repr(CONSTANT) works for CStruct as well';

# LibXML::Dict (t/00dict.t): `dies-ok { $dict<e> := $e }` -- an object that
# stores through ASSIGN-KEY but has no BIND-KEY cannot bind an element.
class Dict {
    has %.h;
    method AT-KEY($k) { %!h{$k} }
    method ASSIGN-KEY($k, $v) { %!h{$k} = $v }
    method elems { %!h.elems }
}
my Dict $d .= new;
my $e = 'e';
dies-ok { $d<e> := $e }, 'binding an element of an ASSIGN-KEY-only object dies';
$d<e> = $e;
is $d.elems, 1, 'the failed bind stored nothing; assignment still works';
