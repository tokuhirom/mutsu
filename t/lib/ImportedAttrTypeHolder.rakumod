unit module ImportedAttrTypeHolder;

use ImportedAttrTypeSource;

class Handle is export {
    has LineParser $.parser is rw = LineParser.new;
}

# A bare mention of the imported type in another class's method.
class Pool is export {
    method fresh { LineParser.new }
}
