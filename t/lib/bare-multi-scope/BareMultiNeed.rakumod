# A package-less module loaded with `need` or `CompUnit::Repository.need`:
# none of its families may reach the loading scope (#11004).
proto sub bmn-proto(|) is export {*}
multi sub bmn-proto(Int) { "int" }
multi sub bmn-proto(Str) { "str" }
multi sub bmn-multi(Int) is export { "m-int" }
multi sub bmn-private(Int) { "p-int" }
