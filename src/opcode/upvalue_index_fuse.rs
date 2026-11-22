//! Fuse a captured array's element read, `GetUpvalue` ... `Index`, into
//! `OpCode::IndexUpvalue` (ADR-18637), so the VM can read the element from
//! the captured cell's published snapshot instead of cloning the array.
use super::*;

/// The most ops a fused subscript may span. A subscript that long is no
/// longer the hot shape this fusion is for.
const MAX_SUBSCRIPT_OPS: usize = 8;

/// Stack effect `(pops, pushes)` of an op allowed inside a fused subscript:
/// a read of a local or a constant, or integer arithmetic on them. None of
/// these can write a captured variable, so evaluating the subscript before
/// the upvalue is read (the fused order) observes the same upvalue as
/// evaluating it after (the source order). Any other op stops the fusion.
// Cost: O(1).
fn pure_subscript_op_effect(op: &OpCode) -> Option<(usize, usize)> {
    match op {
        OpCode::GetLocal(_) | OpCode::LoadConst(_) => Some((0, 1)),
        OpCode::Add | OpCode::Sub | OpCode::Mul | OpCode::BitAnd => Some((2, 1)),
        _ => None,
    }
}

impl CompiledCode {
    /// Rewrite each recorded `@var[subscript]` candidate
    /// (`upvalue_index_candidates`) whose variable read became
    /// `GetUpvalue { index, name_idx }` at `p`, and whose subscript up to the
    /// positional `Index` at `q` is pure subscript ops only, into `Jump(p + 1)`
    /// at `p` and `IndexUpvalue { index, name_idx }` at `q`. The rewrite is in
    /// place, so no op moves and no jump target changes. The compiler records
    /// a candidate only for a single-op variable read, so control enters the
    /// span only at `p`; the pure ops hold no jump either.
    // Cost: O(c * MAX_SUBSCRIPT_OPS), c = candidates.
    pub(crate) fn fuse_upvalue_index_reads(&mut self) {
        let candidates = std::mem::take(&mut self.upvalue_index_candidates);
        for (p, q) in candidates {
            let (p, q) = (p as usize, q as usize);
            if q <= p || q - p > MAX_SUBSCRIPT_OPS + 1 || q >= self.ops.len() {
                continue;
            }
            let OpCode::GetUpvalue { index, name_idx } = self.ops[p] else {
                continue;
            };
            if !matches!(
                self.ops[q],
                OpCode::Index {
                    is_positional: true
                }
            ) {
                continue;
            }
            let mut depth = 0usize;
            let mut fused_at = None;
            for k in p + 1..=q {
                if k == q {
                    if depth == 1 {
                        fused_at = Some(q);
                    }
                    break;
                }
                let Some((pops, pushes)) = pure_subscript_op_effect(&self.ops[k]) else {
                    break;
                };
                // An op that would consume the upvalue's own value is not
                // part of the subscript.
                let Some(after_pops) = depth.checked_sub(pops) else {
                    break;
                };
                depth = after_pops + pushes;
            }
            if let Some(q) = fused_at {
                self.ops[p] = OpCode::Jump((p + 1) as i32);
                self.ops[q] = OpCode::IndexUpvalue { index, name_idx };
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fused(ops: Vec<OpCode>) -> Vec<OpCode> {
        let mut code = CompiledCode::new();
        let last = ops.len() as u32 - 1;
        code.ops = ops;
        code.upvalue_index_candidates = vec![(0, last)];
        code.fuse_upvalue_index_reads();
        code.ops
    }

    #[test]
    fn fuses_a_pure_subscript() {
        let ops = fused(vec![
            OpCode::GetUpvalue {
                index: 0,
                name_idx: 4,
            },
            OpCode::GetLocal(1),
            OpCode::LoadConst(5),
            OpCode::BitAnd,
            OpCode::Index {
                is_positional: true,
            },
        ]);
        assert!(matches!(ops[0], OpCode::Jump(1)));
        assert!(matches!(
            ops[4],
            OpCode::IndexUpvalue {
                index: 0,
                name_idx: 4
            }
        ));
    }

    #[test]
    fn leaves_other_shapes_alone() {
        // An associative subscript.
        let ops = fused(vec![
            OpCode::GetUpvalue {
                index: 0,
                name_idx: 4,
            },
            OpCode::LoadConst(5),
            OpCode::Index {
                is_positional: false,
            },
        ]);
        assert!(matches!(ops[0], OpCode::GetUpvalue { .. }));
        // An op that consumes the upvalue's value before any `Index`.
        let ops = fused(vec![
            OpCode::GetUpvalue {
                index: 0,
                name_idx: 4,
            },
            OpCode::LoadConst(5),
            OpCode::Add,
            OpCode::LoadConst(5),
            OpCode::Index {
                is_positional: true,
            },
        ]);
        assert!(matches!(ops[0], OpCode::GetUpvalue { .. }));
        // A subscript that calls something.
        let ops = fused(vec![
            OpCode::GetUpvalue {
                index: 0,
                name_idx: 4,
            },
            OpCode::Jump(3),
            OpCode::LoadConst(5),
            OpCode::Index {
                is_positional: true,
            },
        ]);
        assert!(matches!(ops[0], OpCode::GetUpvalue { .. }));
    }
}
