//! Compile package trait arguments through the ordinary expression pipeline.

use super::*;

impl Compiler {
    pub(super) fn emit_package_traits(
        &mut self,
        package_idx: u32,
        traits: &[(String, Option<Expr>)],
    ) {
        for (name, argument) in traits {
            match argument {
                Some(argument) => self.compile_expr(argument),
                None => {
                    self.code.emit(OpCode::LoadTrue);
                }
            }
            let trait_idx = self.code.add_constant(Value::str(name.clone()));
            self.code.emit(OpCode::ApplyPackageTrait {
                package_idx,
                trait_idx,
                explicit_argument: argument.is_some(),
            });
        }
    }
}
