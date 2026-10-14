use Test;

class xmlNodeSet is repr('CStruct') is export { has int32 $.size; }
class anyNode is repr('CStruct') is export { has int32 $.size; }

class Holder {
    method accepts($value) {
        $value ~~ xmlNodeSet:D|anyNode:D
    }
}

pass 'lowercase declared type names accept definiteness smileys in a union';

done-testing;
