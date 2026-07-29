unit module DeferredCStruct::Native;
use NativeCall;

constant ptrsize is export = nativesizeof(Pointer);
constant slotint is export = ptrsize == 8 ?? uint64 !! uint32;

class NB is repr('CStruct') is export {
    has slotint $.a is rw;
    has slotint $.b is rw;
}
