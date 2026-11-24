//! The scan's verdicts once the walk is done: which calls an enclosing
//! scope's routine explains, and which `&name` references are judged as
//! calls.

use super::scan::Scan;

impl Scan {
    /// Judge each `&name` reference whose routine the unit declares nowhere,
    /// at any scope, as a call (`say &nosuch` is "Undeclared routine").
    // Cost: O(v + f * r), v = `&name` references, f = frames, r = routines each.
    pub(super) fn adopt_code_vars(&mut self) {
        let code_vars = std::mem::take(&mut self.code_vars);
        // A `&name` carries no scope here: with a `use` below the top level,
        // one inside its scope may name what it imported (#13441).
        if self.frames.iter().any(|f| f.opaque_imports) {
            return;
        }
        let saved_line = self.line;
        for (name, line) in code_vars {
            let declared_somewhere = self.declared.contains(&name)
                || self.body_routines.contains(&name)
                || self.frames.iter().any(|f| f.routines.contains(&name));
            if !declared_somewhere {
                self.line = line;
                self.record_call(&name);
            }
        }
        self.line = saved_line;
    }

    /// Drop every call that a routine declared in an enclosing routine body
    /// explains.
    // Cost: O(c * d), c = calls, d = routine-body nesting depth.
    pub(super) fn resolve_body_scoped_calls(&mut self) {
        let frames = &self.frames;
        let mut call_frames = std::mem::take(&mut self.call_frames).into_iter();
        self.calls.retain(|(name, _)| {
            let mut frame = call_frames.next().flatten();
            while let Some(i) = frame {
                if frames[i].routines.contains(name) || frames[i].opaque_imports {
                    return false;
                }
                frame = frames[i].parent;
            }
            true
        });
    }
}
