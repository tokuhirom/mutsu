use MethodParamDefaultHelper;
class MethodParamDefaultUser is export {
  method pick(Str:D :$name = default-label()) { $name }
  method pick-pos($x, Str:D :$name = default-label() --> Str) { "$x:$name" }
}
