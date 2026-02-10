# S16 - I/O

Reference: `old-design-docs/S16-io.pod`

Covers file I/O, path manipulation, and standard handles.

---

## Standard I/O Handles

- [x] `$*OUT` (standard output) - implicit in `say`/`print`
- [x] `$*ERR` (standard error) - used by `note`
- [ ] `$*IN` (standard input)
- [ ] `$*ARGFILES` (magic input handle)
- [ ] Dynamic scoping override of I/O handles

---

## IO::Path Class

### Path Construction
- [ ] `.IO` coercer on strings
- [ ] `IO::Path.new($path)`

### Path Manipulation
- [ ] `.basename` - filename without directory
- [ ] `.parent` - parent directory
- [ ] `.child($name)` - child path
- [ ] `.extension` - file extension
- [ ] `.volume` - volume (Windows)
- [ ] `.absolute` - absolute path
- [ ] `.relative` - relative path
- [ ] `.resolve` - resolve symlinks
- [ ] `.is-absolute` / `.is-relative`

### File Tests
- [ ] `.e` - exists
- [ ] `.f` - is file
- [ ] `.d` - is directory
- [ ] `.l` - is symlink
- [ ] `.r` / `.w` / `.x` - readable/writable/executable
- [ ] `.s` - file size
- [ ] `.z` - zero size
- [ ] `.modified` / `.accessed` / `.changed` - timestamps

### File Operations
- [ ] `.slurp` - read entire file
- [ ] `.spurt($content)` - write entire file
- [ ] `.lines` - read lines lazily
- [ ] `.words` - read words lazily
- [ ] `.open(:r, :w, :a)` - open file handle
- [ ] `.copy($dest)`, `.rename($dest)`, `.move($dest)`
- [ ] `.unlink` - delete file
- [ ] `.chmod($mode)` - change permissions
- [ ] `.mkdir` - create directory
- [ ] `.rmdir` - remove directory
- [ ] `.dir` - list directory contents

---

## IO::Handle Class

- [ ] `.close` - close handle
- [ ] `.get` - read one line
- [ ] `.getc` - read one character
- [ ] `.lines` - read lines
- [ ] `.words` - read words
- [ ] `.read($bytes)` - read bytes
- [ ] `.write($buf)` - write buffer
- [ ] `.print($str)` - print string
- [ ] `.say($str)` - print with newline
- [ ] `.flush` - flush output
- [ ] `.seek($pos)` / `.tell` - position control
- [ ] `.eof` - end of file
- [ ] `.encoding` - get/set encoding
- [ ] `.opened` - is handle open
- [ ] `.slurp` / `.spurt`

---

## IO::Spec (Path Semantics)

- [ ] `$*SPEC` dynamic variable
- [ ] OS-specific subclasses (Unix, Win32)
- [ ] `.canonpath` - canonicalize path
- [ ] `.catdir`, `.catpath` - join path components
- [ ] `.splitpath`, `.splitdir` - split path
- [ ] `.abs2rel`, `.rel2abs` - path conversion
- [ ] `.curdir`, `.updir`, `.rootdir` - special directories
- [ ] `.devnull` - null device

---

## Functions

### Output
- [x] `print(args)` - print without newline
- [x] `say(args)` - print with newline
- [x] `note(args)` - print to stderr
- [ ] `dd(args)` - debug dump
- [ ] `prompt($msg)` - read with prompt

### File Operations
- [ ] `open($path, :r, :w, :a)` - open file
- [ ] `close($handle)` - close handle
- [x] `slurp($path)` - read entire file
- [x] `spurt($path, $content)` - write file
- [ ] `dir($path)` - list directory

### Directory Operations
- [ ] `mkdir($path)` - create directory
- [ ] `rmdir($path)` - remove directory
- [ ] `chdir($path)` - change directory
- [ ] `indir($path, &code)` - scoped chdir
- [ ] `tmpdir()` - temporary directory
- [ ] `homedir()` - home directory

### File Manipulation
- [ ] `copy($src, $dest)` - copy file
- [ ] `rename($src, $dest)` - rename/move file
- [ ] `move($src, $dest)` - move file
- [x] `unlink($path)` - delete file
- [ ] `chmod($mode, $path)` - change permissions
- [ ] `link($target, $name)` - create hard link
- [ ] `symlink($target, $name)` - create symbolic link

---

## Dynamic Variables

- [ ] `$*CWD` - current working directory (IO::Dir)
- [ ] `$*TMPDIR` - temporary directory
- [ ] `$*HOME` - user home directory
