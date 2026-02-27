use super::*;

impl Interpreter {
    pub(super) fn builtin_chrs(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut result = String::new();
        for arg in args {
            for item in Self::value_to_list(arg) {
                if let Value::Int(i) = item
                    && i >= 0
                    && (i as u64) <= 0x10ffff
                    && let Some(ch) = std::char::from_u32(i as u32)
                {
                    result.push(ch);
                    continue;
                }
                result.push_str(&item.to_string_value());
            }
        }
        Ok(Value::Str(result))
    }

    pub(super) fn builtin_chr(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(Value::Int(i)) = args.first()
            && *i >= 0
            && let Some(ch) = std::char::from_u32(*i as u32)
        {
            return Ok(Value::Str(ch.to_string()));
        }
        Ok(Value::Str(String::new()))
    }

    pub(super) fn builtin_ord(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(val) = args.first()
            && let Some(ch) = val.to_string_value().chars().next()
        {
            return Ok(Value::Int(ch as u32 as i64));
        }
        Ok(Value::Nil)
    }

    pub(super) fn builtin_ords(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(val) = args.first() {
            let codes = val
                .to_string_value()
                .chars()
                .map(|ch| Value::Int(ch as u32 as i64))
                .collect();
            return Ok(Value::array(codes));
        }
        Ok(Value::array(Vec::new()))
    }

    pub(super) fn builtin_flip(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        Ok(Value::Str(val.chars().rev().collect()))
    }

    pub(super) fn builtin_lc(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned().unwrap_or(Value::Nil);
        Ok(Value::Str(val.to_string_value().to_lowercase()))
    }

    pub(super) fn builtin_uc(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args.first().cloned().unwrap_or(Value::Nil);
        Ok(Value::Str(val.to_string_value().to_uppercase()))
    }

    pub(super) fn builtin_tc(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let mut result = String::new();
        let mut capitalize = true;
        for ch in val.chars() {
            if capitalize {
                for c in ch.to_uppercase() {
                    result.push(c);
                }
                capitalize = false;
            } else {
                result.push(ch);
            }
        }
        Ok(Value::Str(result))
    }

    pub(super) fn builtin_trim(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let val = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        Ok(Value::Str(val.trim().to_string()))
    }

    pub(super) fn builtin_chars(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        use unicode_segmentation::UnicodeSegmentation;
        let val = args.first().cloned();
        Ok(match val {
            Some(Value::Str(s)) => Value::Int(s.graphemes(true).count() as i64),
            Some(v) => Value::Int(v.to_string_value().graphemes(true).count() as i64),
            _ => Value::Int(0),
        })
    }

    pub(super) fn builtin_sprintf(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let fmt = match args.first() {
            Some(Value::Str(s)) => s.clone(),
            _ => String::new(),
        };
        let rendered = super::format_sprintf(&fmt, args.get(1));
        Ok(Value::Str(rendered))
    }

    pub(super) fn builtin_make_format(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let fmt = args.first().map(Value::to_string_value).unwrap_or_default();
        let mut attrs = HashMap::new();
        attrs.insert("format".to_string(), Value::Str(fmt));
        Ok(Value::make_instance("Format".to_string(), attrs))
    }
}
