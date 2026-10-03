use super::*;

impl Compiler {
    /// Tag a plain scalar variable argument of a code-object call (`&g($x)`,
    /// `$x.&g`, `.&g`, `$code($x)`) with its variable, as an ordinary
    /// function call's argument is (`compile_call_arg`), so a sigilless or
    /// `is raw` parameter binds the caller's container. The topic in
    /// particular had no other route: `.&g` with `sub g(\s) { s .= chomp }`
    /// (P5chomp) died with "Cannot modify an immutable value".
    pub(super) fn tag_code_call_var_arg(&mut self, arg: &Expr) {
        if let Expr::Var(name) = arg
            && !name.starts_with("__")
        {
            self.emit_wrap_var_ref_arg_tag(name);
        }
    }
}
