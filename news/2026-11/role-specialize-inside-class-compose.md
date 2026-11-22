# A role specializes inside the class HOW's compose override

In rakudo a class's roles are applied, and a role HOW's `specialize` called,
by `Metamodel::ClassHOW.compose`, so a user `compose` override on the class's
HOW runs first and the `specialize` happens inside its `nextsame`. mutsu drove
the role HOW's `specialize` at the end of the class body, before the class
HOW's `compose` override (#18145). When the class HOW carries a `compose`
override the queued `specialize` calls now wait, and the native compose that
the override's `nextsame` reaches runs them.
