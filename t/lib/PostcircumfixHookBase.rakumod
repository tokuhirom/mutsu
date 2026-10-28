unit module PostcircumfixHookBase;

role Node is export {
    method v { 5 }
    method at($i) { "elem $i" }
}
class Index is export does Node { }
class Item is export does Node { }
role Tagged is export { }

multi postcircumfix:<[ ]>(Node $value where { .v == 5 }, Node $index) is export {
    $value.at($index.v)
}
