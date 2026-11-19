//! Junction autothreading of a core routine called in its sub form (#18098).
//!
//! A built-in routine has no registered signature, so the by-name
//! autothreader (`maybe_autothread_func_call`) found no parameters to decide
//! with and declined: `abs(any(-1, -2))` reached the native function, which
//! does not take a Junction, and died with "Unknown function", while `uc` and
//! `sqrt` took the Junction as a plain value. Rakudo threads a Junction
//! argument whenever no candidate of the routine accepts it whole, and its
//! candidates are the snapshot in `builtins::rakudo_candidates`.

/// Whether a Junction argument of the core routine `name` is threaded: the
/// routine has core candidates and none of them takes a Junction whole, that
/// is, none declares a `Mu` or `Junction` parameter, a slurpy (`*@`, `**@`,
/// `+@`, `*%`) or a capture (`|c`), each of which binds the Junction as it is.
// Cost: O(c * s), c = core candidates of `name`, s = signature length; only
// reached for a call that has a Junction argument.
pub(super) fn core_routine_threads_junctions(name: &str) -> bool {
    let rows = crate::builtins::rakudo_candidates::core_sub_candidates(name);
    !rows.is_empty()
        && rows
            .iter()
            .all(|row| !signature_takes_junction(row.signature))
}

/// Whether a candidate signature, as Rakudo prints it (`:(Real:D $a)`), has a
/// parameter that binds a Junction without threading.
// Cost: O(s), s = signature length.
fn signature_takes_junction(signature: &str) -> bool {
    let words = signature
        .split(|c: char| !(c.is_alphanumeric() || c == '_' || c == '-' || c == ':'))
        .filter(|word| !word.is_empty());
    let mut typed = false;
    for word in words {
        let base = word.split(':').next().unwrap_or(word);
        if matches!(base, "Mu" | "Junction") {
            typed = true;
        }
    }
    typed
        || signature.contains("*@")
        || signature.contains("*%")
        || signature.contains("+@")
        || signature.contains('|')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn junction_taking_signatures() {
        assert!(signature_takes_junction(":(Mu $x)"));
        assert!(signature_takes_junction(":(Mu:D \\x)"));
        assert!(signature_takes_junction(":(Junction $j)"));
        assert!(signature_takes_junction(":(*@args)"));
        assert!(signature_takes_junction(":(**@args)"));
        assert!(signature_takes_junction(":(+@values)"));
        assert!(signature_takes_junction(":(|c)"));
        assert!(!signature_takes_junction(":(Real:D $a)"));
        assert!(!signature_takes_junction(":(\\a)"));
        assert!(!signature_takes_junction(":(Int:D \\a --> Int:D)"));
        assert!(!signature_takes_junction(":($s --> Str:D)"));
    }

    #[test]
    fn core_routines() {
        assert!(core_routine_threads_junctions("abs"));
        assert!(core_routine_threads_junctions("chr"));
        assert!(!core_routine_threads_junctions("no-such-core-routine"));
    }
}
