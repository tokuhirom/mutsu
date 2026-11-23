# A qualified type name nothing could end in skips the relative walk

A package-qualified type name can be written relative to an enclosing package
(`A::Item` inside `module Core` means `Core::A::Item`). Resolving one walks
every package enclosing the running code and asks whether `Pkg::name` is a
registered type. The type-check memo's hit path asks this on every check of a
qualified core name such as `Metamodel::GenericHOW`, and the walk always
misses for those names. The interpreter now keeps the set of every proper
`::`-suffix of the registered class, role, enum and subset names, rebuilt
when those tables gain or lose a name, and skips the walk when the name is
not in it. The AttrX::Mooish t/090 probe drops 0.69% Ir per round (#18274).
