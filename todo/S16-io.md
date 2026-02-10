# S16 - I/O

Reference: `old-design-docs/S16-io.pod`

Covers file I/O, path manipulation, and standard handles.

---

## Standard I/O Handles

- [x] `$*OUT` (standard output) - implicit in `say`/`print`
- [x] `$*ERR` (standard error) - used by `note`
- [x] `$*IN` (standard input)
- [x] `$*ARGFILES` (magic input handle)
- [x] Dynamic scoping override of I/O handles

---

## IO::Path Class

### Path Construction
- [x] `.IO` coercer on strings
- [x] `IO::Path.new($path)`

### Path Manipulation
- [x] `.basename` - filename without directory
- [x] `.parent` - parent directory
- [x] `.child($name)` - child path
- [x] `.extension` - file extension
- [x] `.volume` - volume (Windows)
- [x] `.absolute` - absolute path
- [x] `.relative` - relative path
- [x] `.resolve` - resolve symlinks
- [x] `.is-absolute` / `.is-relative`

### File Tests
- [x] `.e` - exists
- [x] `.f` - is file
- [x] `.d` - is directory
- [x] `.l` - is symlink
- [x] `.r` / `.w` / `.x` - readable/writable/executable
- [x] `.s` - file size
- [x] `.z` - zero size
- [x] `.modified` / `.accessed` / `.changed` - timestamps

### File Operations
- [x] `.slurp` - read entire file
- [x] `.spurt($content)` - write entire file
- [x] `.lines` - read lines lazily
- [x] `.words` - read words lazily
- [x] `.open(:r, :w, :a)` - open file handle
- [x] `.copy($dest)`, `.rename($dest)`, `.move($dest)`
- [x] `.unlink` - delete file
- [x] `.chmod($mode)` - change permissions
- [x] `.mkdir` - create directory
- [x] `.rmdir` - remove directory
- [x] `.dir` - list directory contents

---

## IO::Handle Class

- [x] `.close` - close handle
- [x] `.get` - read one line
- [x] `.getc` - read one character
- [x] `.lines` - read lines
- [x] `.words` - read words
- [x] `.read($bytes)` - read bytes
- [x] `.write($buf)` - write buffer
- [x] `.print($str)` - print string
- [x] `.say($str)` - print with newline
- [x] `.flush` - flush output
- [x] `.seek($pos)` / `.tell` - position control
- [x] `.eof` - end of file
- [x] `.encoding` - get/set encoding
- [x] `.opened` - is handle open
- [x] `.slurp` / `.spurt`

---

## IO::Spec (Path Semantics)

- [x] `$*SPEC` dynamic variable
- [x] OS-specific subclasses (Unix, Win32)
- [x] `.canonpath` - canonicalize path
- [x] `.catdir`, `.catpath` - join path components
- [x] `.splitpath`, `.splitdir` - split path
- [x] `.abs2rel`, `.rel2abs` - path conversion
- [x] `.curdir`, `.updir`, `.rootdir` - special directories
- [x] `.devnull` - null device

---

## Functions

### Output
- [x] `print(args)` - print without newline
- [x] `say(args)` - print with newline
- [x] `note(args)` - print to stderr
- [x] `dd(args)` - debug dump
- [x] `prompt($msg)` - read with prompt

### File Operations
- [x] `open($path, :r, :w, :a)` - open file
- [x] `close($handle)` - close handle
- [x] `slurp($path)` - read entire file
- [x] `spurt($path, $content)` - write file
- [x] `dir($path)` - list directory

### Directory Operations
- [x] `mkdir($path)` - create directory
- [x] `rmdir($path)` - remove directory
- [x] `chdir($path)` - change directory
- [x] `indir($path, &code)` - scoped chdir
- [x] `tmpdir()` - temporary directory
- [x] `homedir()` - home directory

### File Manipulation
- [x] `copy($src, $dest)` - copy file
- [x] `rename($src, $dest)` - rename/move file
- [x] `move($src, $dest)` - move file
- [x] `unlink($path)` - delete file
- [x] `chmod($mode, $path)` - change permissions
- [x] `link($target, $name)` - create hard link
- [x] `symlink($target, $name)` - create symbolic link

---

## Dynamic Variables

- [x] `$*CWD` - current working directory (IO::Dir)
- [x] `$*TMPDIR` - temporary directory
- [x] `$*HOME` - user home directory
