// Compiler helper functions, split into submodules by concern.
//
// - helpers_ops: TokenKind to operator name conversion
// - helpers_dynamic: Dynamic scope/variable management
// - helpers_call_args: Call argument compilation, placeholder checks, parameter validation
// - helpers_sub_body: Sub and closure body compilation
// - helpers_control_flow: Try/catch, if-value, body compilation helpers
// - helpers_do_expr: Do-block/if/for/while/loop expression compilation
// - helpers_phasers: Phaser detection, let/temp deep checks, loop phaser expansion
// - helpers_ast_utils: AST inspection, block inline compilation, type checks, heredoc scope

// Re-export for backward compatibility with existing callers.
pub(super) use super::helpers_ops::token_kind_to_op_name;
