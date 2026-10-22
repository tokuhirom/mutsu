# The precompilation cache is private to its user

docs/security.md requires the caches to be "created `0700`/`0600`", but the precompilation
cache and the module scan cache were created with plain `fs::create_dir_all` and `fs::write`.
Their modes came from the umask (`~/.cache/mutsu/precomp` was `0755`, its entries `0644`), and
a write followed a symlink planted at its scratch path. Since ADR-11756 an entry holds bytecode
the next load executes, so a writable cache is a way to run code as the user who loads the
module (#14516).

`src/precomp/private_files.rs` now owns every cache file operation:

- every directory under `<cache>/mutsu` is created `0700`. An existing one is accepted only if
  it is a real directory owned by the current user, and one left looser by an older build is
  tightened;
- an entry is written to a per-writer scratch file opened `O_CREAT|O_EXCL|O_NOFOLLOW` with mode
  `0600`, then renamed into place. The scratch name is now unique per call, not only per
  process, so two threads of one process no longer share it;
- an entry is read through `O_NOFOLLOW` and used only if it is a regular file the current user
  owns and nobody else can write. Anything else is a cache miss.

Pinned by unit tests in the module and by `t/modules/compunit/precomp-cache-private.t`.
