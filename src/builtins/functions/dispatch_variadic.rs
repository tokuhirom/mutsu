#![allow(clippy::result_large_err)]
use super::dispatch_2arg::native_function_2arg;
use super::flat::{deitemize_flat_operand, flat_val, is_infinite_range};
use super::math::{gcd_u64, generic_range_as_bigint, is_extrema_named_pair};
use crate::runtime;
use crate::value::{RuntimeError, Value};
use num_bigint::BigInt as NumBigInt;

pub(crate) fn native_function_variadic(
    name: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    match name {
        "pack" => {
            // pack(Str $template, *@items) — flatten the trailing items so
            // `pack 'S' x $n, $list` (one List arg) sees individual elements.
            let template = args.first().map(Value::to_string_value).unwrap_or_default();
            let mut items = Vec::new();
            for a in args.iter().skip(1) {
                flat_val(a, &mut items, true);
            }
            Some(crate::builtins::pack::pack(&template, &items))
        }
        "unpack" => {
            // unpack(Blob $blob, Str $template)
            let bytes = args
                .first()
                .map(crate::runtime::Interpreter::extract_buf_bytes)
                .unwrap_or_default();
            let template = args.get(1).map(Value::to_string_value).unwrap_or_default();
            Some(crate::builtins::pack::unpack(&bytes, &template))
        }
        "min" => {
            if args.is_empty() {
                return Some(Ok(Value::Nil));
            }
            if args.iter().any(is_extrema_named_pair) {
                return None;
            }
            let mut acc = args[0].clone();
            for rhs in &args[1..] {
                let next = native_function_2arg("min", &acc, rhs)?;
                match next {
                    Ok(v) => acc = v,
                    Err(e) => return Some(Err(e)),
                }
            }
            Some(Ok(acc))
        }
        "max" => {
            if args.is_empty() {
                return Some(Ok(Value::Nil));
            }
            if args.iter().any(is_extrema_named_pair) {
                return None;
            }
            let mut acc = args[0].clone();
            for rhs in &args[1..] {
                let next = native_function_2arg("max", &acc, rhs)?;
                match next {
                    Ok(v) => acc = v,
                    Err(e) => return Some(Err(e)),
                }
            }
            Some(Ok(acc))
        }
        "chrs" => {
            let mut result = String::new();
            let push_chr = |result: &mut String, v: &Value| {
                let code = match v {
                    Value::Int(i) => *i,
                    Value::Num(f) => *f as i64,
                    _ => v.to_string_value().parse::<i64>().unwrap_or(-1),
                };
                if code >= 0
                    && let Some(ch) = std::char::from_u32(code as u32)
                {
                    result.push(ch);
                }
            };
            for arg in args {
                // Flatten Array/Range/Seq/Slip just like the slurpy `*@codes`
                // signature so chrs(72..74) / chrs((72,73,74)) work.
                for item in crate::runtime::utils::value_to_list(arg) {
                    push_chr(&mut result, &item);
                }
            }
            Some(Ok(Value::str(result)))
        }
        "zip" => {
            // zip([@a], [@b], ...) — interleave elements from each list
            // zip takes a single list-of-lists argument; each sub-list is a
            // "column" and zip transposes them into rows.
            let is_lazy_input =
                |v: &Value| -> bool { matches!(v, Value::LazyList(_)) || is_infinite_range(v) };
            let (raw_inputs, single_arg) = if args.len() == 1 {
                (runtime::value_to_list(&args[0]), true)
            } else {
                (args.to_vec(), false)
            };
            let all_lazy = if single_arg {
                // For single-arg zip, check if all sub-lists are lazy
                raw_inputs.iter().all(is_lazy_input)
            } else {
                args.iter().all(is_lazy_input)
            };
            let lists: Vec<Vec<Value>> = raw_inputs.iter().map(runtime::value_to_list).collect();
            if lists.is_empty() {
                return Some(Ok(Value::Seq(std::sync::Arc::new(vec![]))));
            }
            let max_expand: usize = 1_000;
            let min_len = lists
                .iter()
                .map(|l| l.len())
                .min()
                .unwrap_or(0)
                .min(max_expand);
            let mut result = Vec::with_capacity(min_len);
            for i in 0..min_len {
                let row: Vec<Value> = lists.iter().map(|l| l[i].clone()).collect();
                result.push(Value::array(row));
            }
            if all_lazy {
                Some(Ok(Value::LazyList(std::sync::Arc::new(
                    crate::value::LazyList::new_cached(result),
                ))))
            } else {
                // `zip` returns a Seq (so `.^name` is Seq, `.raku` shows `.Seq`),
                // matching Rakudo and the `Z` metaop n-ary path.
                Some(Ok(Value::Seq(std::sync::Arc::new(result))))
            }
        }
        "flat" => {
            if args.len() == 1 && is_infinite_range(&args[0]) {
                return Some(Ok(args[0].clone()));
            }
            // Onearg rule: `flat($(1,2,3))` un-itemizes its sole argument (the
            // slurpy `**@list` treats a single container arg as the list), but
            // `flat(0, $(1,2,3))` keeps each itemized arg as a single element.
            let single = args.len() == 1;
            let mut result = Vec::new();
            for arg in args {
                if single {
                    flat_val(&deitemize_flat_operand(arg), &mut result, true);
                } else {
                    flat_val(arg, &mut result, true);
                }
            }
            Some(Ok(Value::Seq(std::sync::Arc::new(result))))
        }
        "sum" => {
            // If any argument (or element inside an array arg) is a Junction,
            // fold with junction-aware addition
            let has_junction = args.iter().any(|a| match a {
                Value::Junction { .. } => true,
                Value::Array(items, ..) => {
                    items.iter().any(|v| matches!(v, Value::Junction { .. }))
                }
                Value::Seq(items) => items.iter().any(|v| matches!(v, Value::Junction { .. })),
                _ => false,
            });
            if has_junction {
                let items: Vec<Value> = args
                    .iter()
                    .flat_map(|a| match a {
                        Value::Array(items, ..) => items.iter().cloned().collect::<Vec<_>>(),
                        Value::Seq(items) => items.iter().cloned().collect::<Vec<_>>(),
                        other => vec![other.clone()],
                    })
                    .collect();
                let result = items.into_iter().try_fold(
                    Value::Int(0),
                    |acc, item| -> Result<Value, RuntimeError> {
                        crate::builtins::methods_0arg::collection::add_with_junction_threading(
                            acc, item,
                        )
                    },
                );
                return Some(result);
            }
            let mut total: i64 = 0;
            let mut has_num = false;
            let mut total_f: f64 = 0.0;
            for arg in args {
                match arg {
                    Value::Int(i) => {
                        if has_num {
                            total_f += *i as f64;
                        } else {
                            total += i;
                        }
                    }
                    Value::Num(f) => {
                        if !has_num {
                            total_f = total as f64;
                            has_num = true;
                        }
                        total_f += f;
                    }
                    Value::Range(a, b) => {
                        let n = b - a + 1;
                        if n > 0 {
                            let s = n * (a + b) / 2;
                            if has_num {
                                total_f += s as f64;
                            } else {
                                total += s;
                            }
                        }
                    }
                    Value::RangeExcl(a, b) => {
                        let n = b - a;
                        if n > 0 {
                            let b_adj = b - 1;
                            let s = n * (a + b_adj) / 2;
                            if has_num {
                                total_f += s as f64;
                            } else {
                                total += s;
                            }
                        }
                    }
                    Value::RangeExclStart(a, b) => {
                        let start = a + 1;
                        if start <= *b {
                            let n = b - start + 1;
                            let s = n * (start + b) / 2;
                            if has_num {
                                total_f += s as f64;
                            } else {
                                total += s;
                            }
                        }
                    }
                    Value::RangeExclBoth(a, b) => {
                        let start = a + 1;
                        let end = b - 1;
                        if start <= end {
                            let n = end - start + 1;
                            let s = n * (start + end) / 2;
                            if has_num {
                                total_f += s as f64;
                            } else {
                                total += s;
                            }
                        }
                    }
                    Value::GenericRange {
                        start,
                        end,
                        excl_start,
                        excl_end,
                    } => {
                        // Try to detect integer-valued ranges for Gauss formula
                        let start_bi = generic_range_as_bigint(start);
                        let end_bi = generic_range_as_bigint(end);
                        if let (Some(a), Some(b)) = (start_bi, end_bi) {
                            let one = NumBigInt::from(1);
                            let two = NumBigInt::from(2);
                            let zero = NumBigInt::from(0);
                            let eff_start = if *excl_start { &a + &one } else { a };
                            let eff_end = if *excl_end { &b - &one } else { b };
                            if eff_start <= eff_end {
                                let n = &eff_end - &eff_start + &one;
                                let s_plus = &eff_start + &eff_end;
                                let s = if &s_plus % &two == zero {
                                    (&s_plus / &two) * &n
                                } else {
                                    &s_plus * (&n / &two)
                                };
                                if let Ok(val) = i64::try_from(&s) {
                                    if has_num {
                                        total_f += val as f64;
                                    } else {
                                        total += val;
                                    }
                                } else {
                                    // Result is too large for i64, return BigInt directly
                                    return Some(Ok(Value::BigInt(std::sync::Arc::new(s))));
                                }
                            }
                        } else {
                            // Non-integer range: sum via list with Rat support
                            let items = crate::runtime::utils::value_to_list(arg);
                            let items_have_rat =
                                items.iter().any(|v| matches!(v, Value::Rat(_, _)));
                            if items_have_rat {
                                // Use rational arithmetic for the entire result
                                let mut rat_num: i64 = total;
                                let mut rat_den: i64 = 1;
                                for item in &items {
                                    let (in_num, in_den) = match item {
                                        Value::Rat(n, d) => (*n, *d),
                                        Value::Int(n) => (*n, 1),
                                        _ => (crate::runtime::to_int(item), 1),
                                    };
                                    rat_num = rat_num * in_den + in_num * rat_den;
                                    rat_den *= in_den;
                                    let g = gcd_u64(rat_num.unsigned_abs(), rat_den.unsigned_abs())
                                        as i64;
                                    if g > 1 {
                                        rat_num /= g;
                                        rat_den /= g;
                                    }
                                }
                                // Return Rat result directly
                                return if rat_den == 1 {
                                    Some(Ok(Value::Int(rat_num)))
                                } else {
                                    Some(Ok(Value::Rat(rat_num, rat_den)))
                                };
                            }
                            for item in &items {
                                match item {
                                    Value::Int(i) => {
                                        if has_num {
                                            total_f += *i as f64;
                                        } else {
                                            total += i;
                                        }
                                    }
                                    Value::Num(f) => {
                                        if !has_num {
                                            total_f = total as f64;
                                            has_num = true;
                                        }
                                        total_f += f;
                                    }
                                    _ => {
                                        if has_num {
                                            total_f += item.to_f64();
                                        } else {
                                            total += item.to_f64() as i64;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    Value::Array(items, ..) => {
                        for item in items.iter() {
                            if has_num {
                                total_f += item.to_f64();
                            } else if let Value::Num(_) = item {
                                total_f = total as f64 + item.to_f64();
                                has_num = true;
                            } else {
                                total += item.to_f64() as i64;
                            }
                        }
                    }
                    Value::Seq(items) => {
                        for item in items.iter() {
                            if has_num {
                                total_f += item.to_f64();
                            } else if let Value::Num(_) = item {
                                total_f = total as f64 + item.to_f64();
                                has_num = true;
                            } else {
                                total += item.to_f64() as i64;
                            }
                        }
                    }
                    _ => {
                        if has_num {
                            total_f += arg.to_f64();
                        } else {
                            total += arg.to_f64() as i64;
                        }
                    }
                }
            }
            if has_num {
                Some(Ok(Value::Num(total_f)))
            } else {
                Some(Ok(Value::Int(total)))
            }
        }
        _ => None,
    }
}
