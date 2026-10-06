//! `IO::Path`'s `stat` rows (ADR-11276 §9.19): the file tests (`e`, `f`, `d`,
//! `l`, `r`, `w`, `x`, `rw`, `rwx`, `z`) and the readers of what `stat`
//! returns (`s`, `mode`, `inode`, `dev`, `devtype`, `created`, `modified`,
//! `accessed`, `changed`). Each resolves the receiver's path against the cwd
//! and reads the filesystem once; a missing path is a `Failure`, as in Rakudo.

use super::io_path_ctx::PathCtx;
use crate::builtins::method_table::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

/// One zero-argument `stat` row and its handler, named by the method.
macro_rules! stat_rows {
    ($(($name:literal, $handler:ident)),* $(,)?) => {
        $(
            // Cost: O(p) plus one filesystem query, p = chars of the path.
            fn $handler(
                interp: &mut Interpreter,
                target: &Value,
                _args: &[Value],
                _named: Named<'_>,
            ) -> Option<Result<Value, RuntimeError>> {
                let ctx = PathCtx::of(target)?;
                Some(interp.io_path_stat(&ctx.attributes, $name))
            }
        )*

        pub(super) static ROWS: &[MethodRow] = &[
            $(
                MethodRow {
                    owner: "IO::Path",
                    name: $name,
                    arity: 0,
                    handler: Handler::Interp($handler),
                    flags: RowFlags::NONE,
                    named: &[],
                },
            )*
        ];
    };
}

stat_rows!(
    ("e", e_row),
    ("f", f_row),
    ("d", d_row),
    ("l", l_row),
    ("r", r_row),
    ("w", w_row),
    ("x", x_row),
    ("rw", rw_row),
    ("rwx", rwx_row),
    ("z", z_row),
    ("s", s_row),
    ("mode", mode_row),
    ("inode", inode_row),
    ("dev", dev_row),
    ("devtype", devtype_row),
    ("created", created_row),
    ("modified", modified_row),
    ("accessed", accessed_row),
    ("changed", changed_row),
);
