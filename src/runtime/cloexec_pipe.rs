//! Anonymous pipes whose both ends are close-on-exec.
//!
//! Raw `libc::pipe` hands out inheritable fds, so every child process spawned
//! while the pipe is open would hold both ends. `Command`'s stdio setup dup2s
//! the end it wants the child to see onto fd 0/1/2, and dup2 clears the flag on
//! the target, so a pipe created here still reaches the child it is meant for.

// Cost: O(1), one pipe(2)/pipe2(2) call plus at most two fcntl(2) calls.
pub(crate) fn cloexec_pipe() -> std::io::Result<[i32; 2]> {
    let mut fds = [0i32; 2];
    #[cfg(any(target_os = "linux", target_os = "android", target_os = "freebsd"))]
    // SAFETY: `fds` points to two writable file-descriptor slots, as pipe2(2)
    // requires.
    let rc = unsafe { libc::pipe2(fds.as_mut_ptr(), libc::O_CLOEXEC) };
    #[cfg(not(any(target_os = "linux", target_os = "android", target_os = "freebsd")))]
    // SAFETY: `fds` points to two writable file-descriptor slots, as pipe(2)
    // requires.
    let rc = unsafe { libc::pipe(fds.as_mut_ptr()) };
    if rc != 0 {
        return Err(std::io::Error::last_os_error());
    }
    #[cfg(not(any(target_os = "linux", target_os = "android", target_os = "freebsd")))]
    for fd in fds {
        // SAFETY: `fd` was just returned by pipe(2) and is open.
        unsafe {
            let flags = libc::fcntl(fd, libc::F_GETFD);
            libc::fcntl(fd, libc::F_SETFD, flags | libc::FD_CLOEXEC);
        }
    }
    Ok(fds)
}
