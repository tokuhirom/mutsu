# S29 - Built-in Functions

Reference: `old-design-docs/S29-functions.pod`

Covers built-in functions in CORE scope.

---

## Context Functions

- [x] `caller` - caller information
- [x] `callframe` - call frame navigation
- [x] `EVAL($code)` - evaluate code string
- [x] `EVALFILE($path)` - evaluate file
- [x] `exit($code)` - exit program
- [x] `sleep($seconds)` - pause execution
- [x] `sleep-timer($seconds)` - sleep returning remaining time
- [x] `sleep-till($instant)` - sleep until specific time
- [x] `die($message)` - throw exception
- [x] `fail($message)` - lazy exception

---

## Conversion Functions

- [ ] `bless` - create object from Capture
- [x] `chr($codepoint)` - codepoint to character (partial)
- [x] `ord($char)` - character to codepoint (partial)
- [x] `chrs(@codepoints)` - multiple codepoints to string
- [x] `ords($string)` - string to codepoints
- [x] `item($x)` - impose item context
- [x] `list($x)` - impose list context
- [x] `flat(@nested)` - flatten nested lists
- [x] `lol(@lists)` - preserve list-of-lists
- [x] `hash(@pairs)` - create hash from pairs
- [x] `.gist` - human-readable form
- [ ] Radix conversions: `:16($str)`, `:8($str)`, `:2($str)`, `:10($str)`

---

## OS Functions

- [x] `gethost` - hostname lookup
- [ ] `chroot($dir)` - change root directory
- [x] `getlogin` - current login name
- [x] `kill($signal, @pids)` - send signal
- [x] `run($cmd, @args)` - run external command (via Proc::Async)
- [x] `shell($cmd)` - run via shell
- [x] `syscall($num, @args)` - raw system call

---

## Numeric Functions

- [x] `abs($x)` - absolute value
- [x] `sqrt($x)` - square root
- [x] `ceiling($x)` / `ceil($x)` - round up
- [x] `floor($x)` - round down
- [x] `round($x)` - round to nearest
- [x] `sign($x)` - sign (-1, 0, 1)
- [x] `exp($x)` - e^x
- [x] `log($x)` - natural logarithm
- [x] `log($x, $base)` - logarithm with base
- [x] `sin`, `cos`, `tan` - trigonometric functions
- [x] `asin`, `acos`, `atan`, `atan2` - inverse trig
- [x] `truncate($x)` - truncate toward zero
- [ ] `rand` - random number [0, 1)
- [ ] `srand($seed)` - seed random generator

---

## String Functions

- [x] `chars($str)` - grapheme count
- [x] `chomp($str)` - remove trailing newline
- [x] `chop($str)` - remove last character
- [x] `lc($str)` - lowercase
- [x] `uc($str)` - uppercase
- [x] `tc($str)` - titlecase first char
- [ ] `tclc($str)` - titlecase first, lowercase rest
- [x] `flip($str)` - reverse string
- [x] `index($str, $substr)` - find substring
- [x] `rindex($str, $substr)` - find last substring
- [x] `split($sep, $str)` - split string
- [x] `join($sep, @list)` - join list
- [x] `substr($str, $pos, $len)` - substring
- [x] `trim($str)` - remove leading/trailing whitespace
- [ ] `trim-leading($str)` / `trim-trailing($str)`
- [x] `contains($str, $substr)` - containment check

---

## List Functions

- [x] `elems(@list)` - element count
- [x] `sort(@list)` / `sort(&cmp, @list)` - sort
- [x] `reverse(@list)` - reverse
- [x] `unique(@list)` - unique elements
- [x] `squish(@list)` - remove consecutive duplicates
- [x] `first(&pred, @list)` - find first matching
- [x] `grep(&pred, @list)` - filter
- [x] `map(&func, @list)` - transform
- [x] `min(@list)` / `max(@list)` - extremes
- [x] `minmax(@list)` - both extremes
- [x] `sum(@list)` - sum
- [x] `pick($n, @list)` / `roll($n, @list)` - random selection
- [ ] `classify(&func, @list)` - categorize elements
- [ ] `categorize(&func, @list)` - multi-categorize
- [ ] `reduce(&func, @list)` - fold
- [ ] `produce(&func, @list)` - running fold
- [ ] `combinations($n, @list)` - combinations
- [ ] `permutations(@list)` - permutations
- [x] `zip(@a, @b)` - zip lists
- [x] `cross(@a, @b)` - cross product

---

## I/O Functions

- [x] `say(@args)` - print with newline
- [x] `print(@args)` - print without newline
- [x] `note(@args)` - print to stderr
- [ ] `prompt($msg)` - read line with prompt
- [ ] `open($path, :r, :w)` - open file
- [ ] `close($handle)` - close file
- [ ] `slurp($path)` - read file
- [ ] `spurt($path, $content)` - write file
- [ ] `get()` - read one line from $*IN
- [ ] `lines()` - read all lines from $*IN

---

## Concurrency Functions

- [ ] `await($promise)` / `await(@promises)` - wait for results
- [ ] `start { ... }` - start async computation
- [ ] `supply { ... }` - create supply
