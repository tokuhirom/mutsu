use super::*;
use crate::token_kind::TokenKind;

impl Interpreter {
    /// Coerce an Instance operand to a numeric value via its `Numeric`/`Bridge`
    /// method. This is the single authoritative implementation shared by both the
    /// interpreter's infix-routine path (`call_infix_routine`) and the VM's arith/
    /// comparison ops (`VM::coerce_numeric_bridge_value` delegates here), so the
    /// "1 operation = 1 implementation" rule holds for Instance->numeric bridging.
    /// Non-Instance values (the hot Int/Num/Rat path) return early untouched.
    pub(crate) fn coerce_infix_operand_numeric(
        &mut self,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        // A pure positional list (List/Array/Seq) in numeric context coerces to
        // its element count: `(1,2,3) <=> (1,2,4)` is `3 <=> 3` (Same) and
        // `(1,2,3) == (1,2,4)` is `3 == 3` (True), NOT an element-wise compare
        // (that is `cmp`). This must run before the Instance check below so the
        // numeric comparison ops (`== != < > <= >= <=>`) see the elem count.
        if let Some(items) = value.as_list_items() {
            return Ok(Value::Int(items.len() as i64));
        }
        if !matches!(value, Value::Instance { .. }) {
            return Ok(value);
        }
        // Unhandled Failure: throw the stored exception.
        if let Some(err) = self.failure_to_runtime_error_if_unhandled(&value) {
            return Err(err);
        }
        // Match coerces to Numeric via its matched string.
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &value
            && class_name == "Match"
            && let Some(str_val) = attributes.as_map().get("str")
        {
            let s = str_val.to_string_value();
            let s = s.trim();
            if let Ok(i) = s.parse::<i64>() {
                return Ok(Value::Int(i));
            }
            if let Ok(f) = s.parse::<f64>() {
                return Ok(Value::Num(f));
            }
            return Ok(Value::Int(0));
        }
        // Coerce when the type is known Real/Numeric OR the class defines a
        // user `Numeric` method (e.g. `class Blue { method Numeric { 3 } }`).
        let known_numeric =
            self.type_matches_value("Real", &value) || self.type_matches_value("Numeric", &value);
        let has_numeric_method = if let Value::Instance { ref class_name, .. } = value {
            self.has_user_method(&class_name.to_string(), "Numeric")
        } else {
            false
        };
        if !known_numeric && !has_numeric_method {
            return Ok(value);
        }
        self.call_method_with_values(value.clone(), "Numeric", vec![])
            .or_else(|_| self.call_method_with_values(value.clone(), "Bridge", vec![]))
            .or(Ok(value))
    }

    pub(super) fn build_infix_expr(op: &str, left: Value, right: Value) -> Expr {
        if op == "∉" {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal(left)),
                    op: TokenKind::SetElem,
                    right: Box::new(Expr::Literal(right)),
                }),
            };
        }
        if op == "∌" {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal(left)),
                    op: TokenKind::SetCont,
                    right: Box::new(Expr::Literal(right)),
                }),
            };
        }
        if let Some(inner) = op.strip_prefix("![")
            && let Some(inner) = inner.strip_suffix(']')
        {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Self::build_infix_expr(inner, left, right)),
            };
        }
        if let Some(inner) = op.strip_prefix('!')
            && !inner.is_empty()
        {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Self::build_infix_expr(inner, left, right)),
            };
        }
        if op == "⊈" {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Self::build_infix_expr("⊆", left, right)),
            };
        }
        if op == "⊉" {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Self::build_infix_expr("⊇", left, right)),
            };
        }
        if op == "⊄" {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Self::build_infix_expr("⊂", left, right)),
            };
        }
        if op == "⊅" {
            return Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(Self::build_infix_expr("⊃", left, right)),
            };
        }
        Expr::Binary {
            left: Box::new(Expr::Literal(left)),
            op: Self::infix_token(op),
            right: Box::new(Expr::Literal(right)),
        }
    }

    pub(super) fn infix_token(op: &str) -> TokenKind {
        match op {
            "+" => TokenKind::Plus,
            "-" => TokenKind::Minus,
            "*" | "×" => TokenKind::Star,
            "/" | "÷" => TokenKind::Slash,
            "**" => TokenKind::StarStar,
            "%" => TokenKind::Percent,
            "%%" => TokenKind::PercentPercent,
            "!%%" => TokenKind::BangPercentPercent,
            "==" => TokenKind::EqEq,
            "=" => TokenKind::EqEq,
            "!=" => TokenKind::BangEq,
            "===" | "=:=" => TokenKind::EqEqEq,
            "~" => TokenKind::Tilde,
            "+&" => TokenKind::BitAnd,
            "+|" => TokenKind::BitOr,
            "+^" => TokenKind::BitXor,
            "(|)" | "∪" => TokenKind::SetUnion,
            "(+)" | "⊎" => TokenKind::SetAddition,
            "(&)" | "∩" => TokenKind::SetIntersect,
            "(.)" | "⊍" => TokenKind::SetMultiply,
            "(-)" | "∖" => TokenKind::SetDiff,
            "(^)" | "⊖" => TokenKind::SetSymDiff,
            "(elem)" | "∈" => TokenKind::SetElem,
            "(cont)" | "∋" => TokenKind::SetCont,
            "(<=)" | "⊆" => TokenKind::SetSubset,
            "(>=)" | "⊇" => TokenKind::SetSuperset,
            "(<)" | "⊂" => TokenKind::SetStrictSubset,
            "(>)" | "⊃" => TokenKind::SetStrictSuperset,
            "..." => TokenKind::DotDotDot,
            "...^" => TokenKind::DotDotDotCaret,
            ".." => TokenKind::DotDot,
            "<=>" => TokenKind::LtEqGt,
            "<" => TokenKind::Lt,
            ">" => TokenKind::Gt,
            "<=" => TokenKind::Lte,
            ">=" => TokenKind::Gte,
            "~~" => TokenKind::SmartMatch,
            "&&" => TokenKind::AndAnd,
            "||" => TokenKind::OrOr,
            "^^" => TokenKind::XorXor,
            "//" => TokenKind::SlashSlash,
            _ => TokenKind::Ident(op.to_string()),
        }
    }

    /// Apply a hyper meta-operator elementwise. `inner` is the inner operator
    /// (e.g. `+`), and `dwim_left`/`dwim_right` indicate which side may be
    /// recycled (the pointy end of `»`/`«`).
    pub(crate) fn apply_hyper_infix(
        &mut self,
        inner: &str,
        dwim_left: bool,
        dwim_right: bool,
        left: &Value,
        right: &Value,
    ) -> Result<Value, RuntimeError> {
        let left_list = Self::value_to_list(left);
        let right_list = Self::value_to_list(right);
        let left_len = left_list.len();
        let right_len = right_list.len();
        if left_len == 0 && right_len == 0 {
            return Ok(Value::array(Vec::new()));
        }
        let result_len = if !dwim_left && !dwim_right {
            if left_len != right_len {
                return Err(RuntimeError::new(format!(
                    "Non-dwimmy hyper operator: left has {} elements, right has {}",
                    left_len, right_len
                )));
            }
            left_len
        } else if dwim_left && dwim_right {
            std::cmp::max(left_len, right_len)
        } else if dwim_right {
            left_len
        } else {
            right_len
        };
        let mut results = Vec::with_capacity(result_len);
        for i in 0..result_len {
            let l = if left_len == 0 {
                Value::Int(0)
            } else {
                left_list[i % left_len].clone()
            };
            let r = if right_len == 0 {
                Value::Int(0)
            } else {
                right_list[i % right_len].clone()
            };
            let infix_name = format!("infix:<{}>", inner);
            let pair_result = if let Some(v) =
                self.resolve_function_with_types(&infix_name, &[l.clone(), r.clone()])
            {
                self.call_function_def(&v, &[l, r])?
            } else {
                Self::apply_reduction_op(inner, &l, &r)?
            };
            results.push(pair_result);
        }
        // Preserve List kind when the inputs are Lists (not real Arrays), so the
        // function form `infix:<»+«>((1,2,3),(4,5,6))` returns a List just like
        // the operator form `(1,2,3) »+« (4,5,6)`.
        let left_is_array = matches!(left, Value::Array(_, crate::value::ArrayKind::Array));
        let right_is_array = matches!(right, Value::Array(_, crate::value::ArrayKind::Array));
        if !left_is_array && !right_is_array {
            Ok(Value::Array(
                std::sync::Arc::new(crate::value::ArrayData::new(results)),
                crate::value::ArrayKind::List,
            ))
        } else {
            Ok(Value::real_array(results))
        }
    }
}
