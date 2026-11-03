# `has Inp() $.x .= new` calls `new` on the coercion's target type

An attribute declared with a coercion type and a `.= new` default
(`has Inp() $.input .= new`) made the parser build a method call on the
bareword `Inp()`, which neither executed nor round-tripped through RakuAST
(`HarfBuzz::Subset` failed to load). The `.=` default now strips the coercion
suffix (`Inp()`, `Int(Str)`) and calls the method on the target type, as
rakudo does. Issue #15845.
