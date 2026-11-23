//! `CLIENT::`: the code that called into the running code's package.
//!
//! Rakudo walks the dynamic scope outward from the running code and takes
//! the first frame whose package is not the running code's own. Every frame
//! counts, a block included: a `.map` block of a module runs after the
//! routine that built the `Seq` returned, yet it is still the module's code
//! calling back (#18845). A mutsu frame names its caller's code
//! (`VmCallFrame::caller_code`), and a routine or closure body records the
//! package it is declared in (`CompiledCode::package`), so the walk reads
//! the same chain, light calls included (they push a frame too).

use super::*;

impl Interpreter {
    /// The client of the running `code`: the package and language-revision
    /// letter of the first calling code declared in another package. `None`
    /// when `code` records no package, or the walk reaches code that records
    /// none (a unit's mainline) or runs out of frames first; the routine-frame
    /// walk (`client_frame`) answers those.
    // Cost: O(f), f = call frames walked, one revision-table probe.
    pub(super) fn client_code(&self, code: &CompiledCode) -> Option<(Symbol, char)> {
        let own = code.package?;
        for frame in self.call_frames.iter().rev() {
            if frame.caller_code == 0 {
                return None;
            }
            // SAFETY: `caller_code` is the address of the `CompiledCode` the
            // caller was running when it pushed this frame; that caller is
            // suspended until the frame pops, so its code is alive while the
            // frame is on `call_frames`.
            let caller = unsafe { &*(frame.caller_code as *const CompiledCode) };
            match caller.package {
                Some(package) if package == own => continue,
                Some(package) => {
                    let revision = Self::unit_revision_letter(caller.source_file)
                        .unwrap_or_else(|| self.running_revision_letter());
                    return Some((package, revision));
                }
                None => return None,
            }
        }
        None
    }
}
