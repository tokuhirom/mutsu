unit class RequireInRoutineLexTypes;
my constant $Lock = Lock.new;
my constant @Names = <a b c>;
my class COSAttr {
    my class CosOfAttr { method name { 'cos-of' } }
    method of-att { CosOfAttr.new.name }
    method locked { $Lock.protect: { 'locked' } }
    method names { @Names.join(',') }
}
method attr { COSAttr.new }
my $v = 7;
my class Outer {
    my class Inner { method hi { "inner" } }
    method m { "$v " ~ Inner.hi }
}
method outer { Outer.new }
