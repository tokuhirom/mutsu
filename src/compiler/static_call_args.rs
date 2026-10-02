//! Which call sites rakudo's optimizer checks against the callee's signature
//! at compile time (#10640). Rakudo raises the compile-time
//! `X::TypeCheck::Argument` ("Calling f(Str) will never work with declared
//! signature (Int $i)") only when it knows every argument's type statically;
//! any other binding failure is the run-time
//! `X::TypeCheck::Binding::Parameter`. mutsu binds at run time either way, so
//! the call site records which of the two shapes it is
//! (`OpCode::CallFunc::static_arg_types`) and the binder picks the exception
//! from it.
use super::*;
use crate::value::ValueView;

impl Compiler {
    /// Whether every argument of a call has a type known at compile time: a
    /// numeric/string/`Bool`/`Nil` literal, a type object, or a variable
    /// declared with a nominal type (`my Str $s; f($s)`). A named argument, a
    /// flattened one, or any computed expression (`f(-1)`, `f(1 + 1)`,
    /// `f(G.new)`, `f($untyped)`) makes the whole call a run-time one, as in
    /// rakudo (`raku -e 'sub f(Str $) {}; f(1 + 1)'` dies at run time with
    /// `X::TypeCheck::Binding::Parameter`, while `f(2)` is a compile error).
    ///
    /// Known gap: the parser folds `pi`/`e`/`tau` into numeric literals, so
    /// `f(pi)` is treated as static here where rakudo checks it at run time.
    pub(super) fn static_arg_types(&self, args: &[Expr]) -> bool {
        args.iter().all(|arg| self.is_static_typed_arg(arg))
    }

    fn is_static_typed_arg(&self, arg: &Expr) -> bool {
        match arg {
            Expr::Literal(v) => matches!(
                v.view(),
                ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Num(_)
                    | ValueView::Rat(..)
                    | ValueView::Str(_)
                    | ValueView::Bool(_)
                    | ValueView::Nil
            ),
            // A type object (`F`). A bareword is also how a no-paren
            // zero-argument sub call parses, so only a name the parser
            // registered as a type, or a core type, counts. A definite-type
            // object (`Str:D`) is a run-time check in rakudo.
            Expr::BareWord(name) => {
                !name.contains(':')
                    && (crate::parser::is_user_declared_type(name)
                        || crate::runtime::utils::is_known_type_constraint(name))
            }
            // The synthetic marker the parser appends to a parenthesized
            // zero-argument call (`f()`) is not an argument at all.
            Expr::Binary {
                left,
                op: TokenKind::FatArrow,
                ..
            } if matches!(left.as_ref(), Expr::Literal(v)
                if matches!(v.view(), ValueView::Str(s)
                    if s.as_str() == crate::parser::TEST_CALLSITE_LINE_KEY)) =>
            {
                true
            }
            Expr::Var(name) => self
                .local_types
                .get(name)
                .is_some_and(|tc| !tc.contains('(') && !tc.contains('[')),
            _ => false,
        }
    }
}
