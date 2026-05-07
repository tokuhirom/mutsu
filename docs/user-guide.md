# mutsu User Guide

A practical guide to using mutsu, a Raku interpreter written in Rust.

For installation and project overview, see the [README](../README.md).

## Installation

### From source (recommended)

```bash
git clone https://github.com/tokuhirom/mutsu.git
cd mutsu
cargo build --release
```

The binary is at `./target/release/mutsu`. Copy it to your PATH or run it directly.

**Requirements:** Rust 1.92+ and a C compiler (for pcre2-sys).

### GitHub releases

Pre-built binaries are available on the [Releases page](https://github.com/tokuhirom/mutsu/releases).

## Basic Usage

### Running a script

```bash
mutsu script.raku
```

### Inline code with `-e`

```bash
mutsu -e 'say "Hello, World!"'
```

### Interactive REPL

```bash
mutsu --repl
```

### Module search paths

Use `-I` to add directories to the module search path:

```bash
mutsu -I lib script.raku
```

Or set the `MUTSULIB` environment variable (colon-separated):

```bash
MUTSULIB=/path/to/lib1:/path/to/lib2 mutsu script.raku
```

### Pre-loading modules with `-M`

```bash
mutsu -M Test -e 'ok True, "works"'
```

### AST dump (debugging)

```bash
mutsu --dump-ast -e 'say 42'
```

### Pod documentation rendering

```bash
mutsu --doc script.raku
```

## Supported Syntax

### Variables

All four Raku sigils work: `$` (scalar), `@` (array), `%` (hash), `&` (code).

```raku
my $name = "Alice";
my @numbers = 1, 2, 3;
my %ages = alice => 30, bob => 25;
my &fn = sub ($x) { $x * 2 };
say &fn(21);   # 42
```

Special variables are available:

```raku
say %*ENV<HOME>;        # environment variables
say $*PROGRAM-NAME;     # current script name
```

### Basic Types

Int, Rat, Num, Complex, Str, Bool, Array, Hash, Range, Set, Bag, Mix, and Pair.

```raku
say 3/7;                # Rat: 0.428571
say (3/7).nude;         # (3 7)
say 2+3i;               # Complex
say (2+3i).abs;         # 3.605551275463989
my $set = set <a b c>;
say "b" (elem) $set;    # True
my $bag = bag <a a b c c c>;
say $bag<c>;             # 3
```

### String Operations

```raku
say "hello".uc;                  # HELLO
say "HELLO".lc;                  # hello
say "hello world".tc;            # Hello world
say "hello".chars;               # 5
say "hello".flip;                # olleh
say "hello".comb.list;           # (h e l l o)
say "hello world".words.list;    # (hello world)
say "a,b,c".split(",").list;     # (a b c)
say <a b c>.join("-");           # a-b-c
say "hello".subst(/l/, "L", :g); # heLLo
say "Hello, World!".trans("aeiou" => "AEIOU");  # HEllO, WOrld!
```

### Control Flow

```raku
# if/elsif/else
my $x = 42;
if $x > 100    { say "big" }
elsif $x > 10  { say "medium" }
else           { say "small" }

# for loop
for 1..5 -> $i { print "$i " }   # 1 2 3 4 5

# while loop
my $n = 0;
while $n < 3 { say $n; $n++ }

# C-style loop
loop (my $i = 0; $i < 3; $i++) { say $i }

# given/when
given 42 {
    when 0..10   { say "small"  }
    when 11..100 { say "medium" }
    default      { say "big"    }
}
```

### Exception Handling

```raku
try {
    die "something went wrong";
    CATCH { default { say "Caught: {$!.message}" } }
}
```

### Functions

```raku
# Named sub with signature
sub greet(Str $name) { say "Hello, $name!" }
greet("World");

# Multi dispatch
multi sub fizz(Int $n where * %% 15) { "FizzBuzz" }
multi sub fizz(Int $n where * %% 3)  { "Fizz" }
multi sub fizz(Int $n where * %% 5)  { "Buzz" }
multi sub fizz(Int $n)               { $n }
say fizz(15);   # FizzBuzz

# Anonymous blocks and closures
my $square = -> $x { $x * $x };
say $square(5);   # 25

sub make-counter() {
    my $n = 0;
    return sub { $n++; $n }
}
my $c = make-counter();
say $c();   # 1
say $c();   # 2
```

### Classes, Roles, and Inheritance

```raku
class Point {
    has $.x;
    has $.y;
    method distance() { sqrt(self.x ** 2 + self.y ** 2) }
}
my $p = Point.new(x => 3, y => 4);
say $p.distance;   # 5

# Inheritance
class Animal { has $.name }
class Dog is Animal {
    method speak() { say "{self.name} says Woof!" }
}
Dog.new(name => "Rex").speak;

# Roles
role Greetable {
    method greet() { say "Hello, I am {self.name}" }
}
class Person does Greetable { has $.name }
Person.new(name => "Alice").greet;
```

### Enums and Subset Types

```raku
enum Color <Red Green Blue>;
say Red;            # Red
say Green.value;    # 1

subset Positive of Int where * > 0;
my Positive $x = 5;
```

### Regex and Grammars

```raku
# Basic regex matching
if "Hello123" ~~ /(\w+)(\d+)/ {
    say ~$0;   # Hello12
    say ~$1;   # 3
}

# Substitution
my $x = "Hello World";
$x ~~ s/World/Raku/;
say $x;   # Hello Raku

# Grammars
grammar CSV {
    token TOP  { <line>+ % "\n" }
    token line { <cell>+ % ","  }
    token cell { <-[,\n]>*      }
}
say CSV.parse("a,b,c").so;   # True
```

### Functional Programming

```raku
say (1..10).map(* ** 2).list;                # (1 4 9 16 25 36 49 64 81 100)
say (1..10).grep(* > 5).map(* * 2).list;    # (12 14 16 18 20)
say (1..10).grep(*.is-prime);                # (2 3 5 7)
say [+] 1..100;                              # 5050
say (1, 1, *+* ... *)[^10];                 # Fibonacci: (1 1 2 3 5 8 13 21 34 55)
say gather { for 1..10 { take $_ if $_ %% 3 } }.list;   # (3 6 9)
say (1..10).classify({ $_ %% 2 ?? "even" !! "odd" });
# {even => [2 4 6 8 10], odd => [1 3 5 7 9]}
```

### Array and Hash Operations

```raku
my @a = 1, 2, 3;
@a.push(4);
@a.unshift(0);
say @a;              # [0 1 2 3 4]
say @a.elems;        # 5

my %h = a => 1, b => 2, c => 3;
say %h.keys.sort.list;     # (a b c)
say %h.values.sort.list;   # (1 2 3)

for @a.kv -> $i, $v { say "$i: $v" }
```

### I/O

```raku
# File I/O
spurt "output.txt", "Hello from mutsu\n";
say slurp("output.txt").chomp;   # Hello from mutsu

# IO::Path
say "src/".IO.d;    # True (directory exists)

# External commands
my $output = qqx/echo hello/;
say $output.chomp;   # hello
```

### Modules

```raku
# In lib/Greeter.rakumod:
#   unit class Greeter;
#   method hello($name) { say "Hello, $name!" }

use lib 'lib';    # or use -I lib
use Greeter;
Greeter.new.hello("World");
```

### Concurrency

```raku
my $p = start { 21 * 2 };
say await $p;   # 42
```

Note: `start`/`await` work but execute on a single thread.

### Test Module

```raku
use Test;

plan 3;
ok True, "truth";
is 1 + 1, 2, "addition";
is-deeply [1, 2, 3], [1, 2, 3], "arrays match";
done-testing;
```

Available test functions: `plan`, `ok`, `nok`, `is`, `isnt`, `is-deeply`, `is-approx`, `cmp-ok`, `isa-ok`, `does-ok`, `can-ok`, `like`, `unlike`, `dies-ok`, `lives-ok`, `eval-dies-ok`, `eval-lives-ok`, `throws-like`, `subtest`, `skip`, `todo`, `pass`, `flunk`, `bail-out`, `done-testing`, `diag`.

### MAIN Sub for CLI Tools

```raku
# greet.raku
sub MAIN(Str $name) {
    say "Hello, $name!";
}
```

```
$ mutsu greet.raku World
Hello, World!
$ mutsu greet.raku
Usage:
  greet.raku <name>
```

### Junctions

```raku
my $j = any(1, 2, 3);
say so $j == 2;   # True
say so all(2, 4, 6) %% 2;   # True
```

### Phasers

`BEGIN`, `INIT`, `ENTER`, `LEAVE`, `FIRST`, `NEXT`, `LAST`, `END`, and more.

## Differences from Raku

mutsu aims for Raku compatibility but has some known limitations.

### Single-threaded execution

`start` and `await` work but run on a single thread. There is no true parallelism. `hyper` and `race` are parsed but execute sequentially.

### Container semantics

Some binding (`\`) and container behaviors differ from Rakudo. Proxy containers and custom container types are not supported. Most typical variable usage works correctly.

### Grammar actions (make/made)

Grammar parsing works. Action classes with `make`/`$/.made` are partially implemented but may not work correctly in all cases. Simple grammars without actions work well.

### Exception types

Not all `X::` exception classes are implemented. `die`, `try`, and `CATCH` with `default` work. Typed exception matching (`when X::AdHoc`) has limited coverage.

### Missing or incomplete features

- **NativeCall** is not supported.
- **supply/react/whenever** patterns are not implemented.
- **Native types** (`int`, `num`, `str`) are partially supported.
- **Some advanced regex features** (lookbehind, code assertions) may be incomplete.
- **Meta-programming** (`MONKEY-TYPING`, `augment class`) is limited.
- **Format/sprintf** covers common cases but not all directives.

### Module ecosystem

There is no zef or ecosystem integration. `use` works for local modules. Some pure-Raku modules like `JSON::Tiny` can work if placed in a search path. Use `-I` or `MUTSULIB` to point to module directories.

### Performance characteristics

- **Fast startup** compared to Rakudo (bytecode VM in Rust).
- **Computation speed** varies; some operations are slower than Rakudo for large workloads due to the single-threaded constraint and interpreter overhead.
- Suitable for scripting, CLI tools, and learning Raku syntax.

### Error messages

Runtime errors do not yet include line numbers or stack traces. This is being actively worked on (see [PLAN.md](../PLAN.md) Phase 1).

## WebAssembly

mutsu compiles to WebAssembly. Try it in your browser at <https://tokuhirom.github.io/mutsu/>.

## Further Reading

- [README](../README.md) -- project overview and quick start
- [Raku documentation](https://docs.raku.org/) -- official language reference
- [Roast test suite](https://github.com/Raku/roast) -- official Raku specification tests
