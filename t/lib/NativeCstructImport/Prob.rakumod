use NativeCall;
unit module NativeCstructImport::Prob;
my class NativeCstructImport::CProb:auth<x:y>:ver<0.0.1> is export is repr('CStruct') {
    has int32 $.l;
}
