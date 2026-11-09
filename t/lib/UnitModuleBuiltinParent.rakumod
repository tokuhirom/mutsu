unit module UnitModuleBuiltinParent;
class NOT-SET is Nil is export(:NOT-SET) { method Bool { False } }
class MyFail is Failure is export { }
class Version is export { method hi { "mine" } }
class V2 is Version is export { }
