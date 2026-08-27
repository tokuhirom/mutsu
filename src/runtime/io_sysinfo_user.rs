use super::*;

impl Interpreter {
    /// `$*USER` -- the effective user's login name and numeric uid, exposed as
    /// an `IntStr` allomorph: `say $*USER` prints the login name while
    /// `+$*USER` / `$*USER == 0` reads the numeric uid (a permission check
    /// almost always wants the numeric form, which is why this silently read
    /// as `Nil`/`0` before -- see
    /// todo/tickets/user-group-dynamic-variables-missing.md). Rakudo builds
    /// this from `getpwuid(geteuid())`. Built lazily on first read (the
    /// `$*DISTRO`/`$*KERNEL` pattern -- see
    /// `Interpreter::lazy_magic_dynamic_var` in `io_env.rs`), and cached for
    /// the life of the process the same way.
    pub(super) fn make_user_instance() -> Value {
        Self::make_id_allomorph(Self::euid_and_login_name)
    }

    /// `$*GROUP` -- the effective-gid twin of [`Self::make_user_instance`].
    pub(super) fn make_group_instance() -> Value {
        Self::make_id_allomorph(Self::egid_and_group_name)
    }

    /// Shared allomorph assembly: `lookup()` returns `(id, Some(name))` when
    /// the passwd/group database has an entry for the effective uid/gid, or
    /// `(id, None)` when it does not -- a uid/gid with no passwd/group entry
    /// is normal in containers. Rakudo falls back to the bare numeric id with
    /// no `Str` facet in that case (a plain `Int`, not an `IntStr` with an
    /// empty string), so `+$*USER` still reports the real uid even when the
    /// name can't be resolved, rather than silently reading as `0`.
    fn make_id_allomorph(lookup: fn() -> (i64, Option<String>)) -> Value {
        let (id, name) = lookup();
        match name {
            Some(name) => {
                Self::build_native_allomorph_value("IntStr", &[Value::int(id), Value::str(name)])
                    .unwrap_or_else(|_| Value::int(id))
            }
            None => Value::int(id),
        }
    }

    /// The effective uid and (if resolvable) login name, via
    /// `geteuid()`/`getpwuid_r()`. Miri cannot call a foreign function, so it
    /// (like every other libc-backed magic var -- see
    /// `local_timezone_offset_secs`, `io_sysinfo_host`) takes the "no name"
    /// fallback rather than aborting the interpreter it is trying to check;
    /// wasm32 has no process/user model at all and takes the same fallback.
    #[cfg(all(unix, not(miri), not(target_arch = "wasm32"), feature = "native"))]
    fn euid_and_login_name() -> (i64, Option<String>) {
        let uid = unsafe { libc::geteuid() };
        (uid as i64, Self::getpwuid_name(uid))
    }

    #[cfg(not(all(unix, not(miri), not(target_arch = "wasm32"), feature = "native")))]
    fn euid_and_login_name() -> (i64, Option<String>) {
        // mutsu ships no Windows target, so this arm only needs to compile
        // for wasm32 / miri / a `native`-feature-disabled build -- report a
        // uid with no resolvable name, the same fallback shape as a uid with
        // no passwd entry on a real POSIX host.
        (0, None)
    }

    #[cfg(all(unix, not(miri), not(target_arch = "wasm32"), feature = "native"))]
    fn egid_and_group_name() -> (i64, Option<String>) {
        let gid = unsafe { libc::getegid() };
        (gid as i64, Self::getgrgid_name(gid))
    }

    #[cfg(not(all(unix, not(miri), not(target_arch = "wasm32"), feature = "native")))]
    fn egid_and_group_name() -> (i64, Option<String>) {
        (0, None)
    }

    /// `getpwuid_r(3)`, the reentrant passwd lookup. Returns `None` when the
    /// uid has no passwd entry instead of erroring -- that is the documented
    /// Rakudo fallback (a bare numeric `$*USER`).
    #[cfg(all(unix, not(miri), not(target_arch = "wasm32"), feature = "native"))]
    fn getpwuid_name(uid: libc::uid_t) -> Option<String> {
        // SAFETY: `passwd` is a plain C struct; an all-zero value is a valid
        // initialization for a pointee `getpwuid_r` only ever writes into.
        let mut pwd: libc::passwd = unsafe { std::mem::zeroed() };
        let mut buf = vec![0u8; 4096];
        let mut result: *mut libc::passwd = std::ptr::null_mut();
        loop {
            // SAFETY: `pwd`/`buf`/`result` are valid, correctly-sized
            // out-parameters for the reentrant `_r` form.
            let ret = unsafe {
                libc::getpwuid_r(
                    uid,
                    &mut pwd,
                    buf.as_mut_ptr().cast(),
                    buf.len(),
                    &mut result,
                )
            };
            if ret == libc::ERANGE {
                // The scratch buffer was too small for this system's passwd
                // strings; double it and retry rather than guessing a size.
                buf.resize(buf.len() * 2, 0);
                continue;
            }
            break;
        }
        if result.is_null() {
            return None;
        }
        // SAFETY: a non-null `result` means `pwd.pw_name` points into `buf`,
        // which stays alive for the rest of this function.
        let name = unsafe { std::ffi::CStr::from_ptr(pwd.pw_name) };
        Some(name.to_string_lossy().into_owned())
    }

    /// `getgrgid_r(3)`, the group twin of [`Self::getpwuid_name`].
    #[cfg(all(unix, not(miri), not(target_arch = "wasm32"), feature = "native"))]
    fn getgrgid_name(gid: libc::gid_t) -> Option<String> {
        // SAFETY: see `getpwuid_name` -- same reentrant-lookup shape.
        let mut grp: libc::group = unsafe { std::mem::zeroed() };
        let mut buf = vec![0u8; 4096];
        let mut result: *mut libc::group = std::ptr::null_mut();
        loop {
            let ret = unsafe {
                libc::getgrgid_r(
                    gid,
                    &mut grp,
                    buf.as_mut_ptr().cast(),
                    buf.len(),
                    &mut result,
                )
            };
            if ret == libc::ERANGE {
                buf.resize(buf.len() * 2, 0);
                continue;
            }
            break;
        }
        if result.is_null() {
            return None;
        }
        let name = unsafe { std::ffi::CStr::from_ptr(grp.gr_name) };
        Some(name.to_string_lossy().into_owned())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::ValueView;

    /// The real effective uid resolves to a name on this box (and on CI),
    /// producing an `IntStr` allomorph -- exercises the "happy path" arm of
    /// `make_id_allomorph` directly, independent of the interpreter's
    /// dynamic-var plumbing.
    #[test]
    #[cfg(all(unix, not(miri), not(target_arch = "wasm32"), feature = "native"))]
    fn euid_resolves_to_an_intstr_allomorph() {
        let user = Interpreter::make_user_instance();
        match user.view() {
            ValueView::Mixin(inner, mixins) => {
                assert!(matches!(inner.view(), ValueView::Int(_)));
                let name = mixins.get("Str").expect("Str facet");
                assert!(!name.to_string_value().is_empty());
            }
            other => panic!("expected a Mixin allomorph, got {other:?}"),
        }
    }

    /// A uid with no passwd entry (a made-up value chosen far outside any
    /// real allocation range) is Rakudo's documented fallback case: the
    /// numeric id alone, with no `Str` facet -- i.e. a plain `Int`, not an
    /// `IntStr` with an empty string. This is the code path this repo cannot
    /// otherwise exercise without a genuinely absent passwd entry (containers
    /// vary), so it is driven directly.
    #[test]
    fn missing_passwd_entry_falls_back_to_bare_int() {
        let value = Interpreter::make_id_allomorph(|| (999_999_999, None));
        assert!(matches!(value.view(), ValueView::Int(999_999_999)));
    }

    /// The mirror case: a resolvable name builds the `IntStr` allomorph with
    /// both facets, regardless of which lookup (`$*USER` vs `$*GROUP`) feeds
    /// it.
    #[test]
    fn resolvable_name_builds_intstr_allomorph() {
        let value = Interpreter::make_id_allomorph(|| (1000, Some("someuser".to_string())));
        match value.view() {
            ValueView::Mixin(inner, mixins) => {
                assert!(matches!(inner.view(), ValueView::Int(1000)));
                assert_eq!(
                    mixins.get("Str").expect("Str facet").to_string_value(),
                    "someuser"
                );
            }
            other => panic!("expected a Mixin allomorph, got {other:?}"),
        }
    }

    /// `getpwuid_r`/`getgrgid_r` for a uid/gid chosen far outside any real
    /// allocation range must resolve to "no entry", not panic or loop
    /// forever on `ERANGE`.
    #[test]
    #[cfg(all(unix, not(miri), not(target_arch = "wasm32"), feature = "native"))]
    fn nonexistent_uid_and_gid_resolve_to_none() {
        assert_eq!(Interpreter::getpwuid_name(u32::MAX - 1), None);
        assert_eq!(Interpreter::getgrgid_name(u32::MAX - 1), None);
    }
}
