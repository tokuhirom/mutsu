# Loaded through `CompUnit::Repository::FileSystem.need` (#11004).
proto sub bmr-proto(|) is export {*}
multi sub bmr-proto(Int) { "int" }
multi sub bmr-proto(Str) { "str" }
multi sub bmr-multi(Int) is export { "m-int" }
multi sub bmr-private(Int) { "p-int" }
