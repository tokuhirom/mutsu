# mutsu

A Raku (Perl 6) interpreter written in Rust, using a bytecode VM architecture.

mutsu parses Raku source into an AST, compiles it to bytecode, and executes it on a custom VM. It is under active development and improving rapidly, but is **not yet suitable for production use**.

Try it in your browser: **https://tokuhirom.github.io/mutsu/** (WebAssembly demo)

## Quick Start

```bash
cargo build --release
./target/release/mutsu -e 'say "Hello, World!"'
./target/release/mutsu script.raku
```

For interactive use:

```bash
./target/release/mutsu --repl
```

## Status

mutsu passes **1,145 out of ~1,460** official [Roast](https://github.com/Raku/roast) test files. Compatibility is improving daily.

## What Works

### Variables and Basic Types

Int, Str, Rat, Num, Complex, Bool, Array, Hash, Range, Set, Bag, and Mix are all supported.

```raku
my $name = "Alice";
my @numbers = 1, 2, 3;
my %ages = alice => 30, bob => 25;
my $ratio = 3/7;            # Rat: 0.428571
my $z = 2+3i;               # Complex
my $set = set <a b c>;
say "b" (elem) $set;        # True
```

### Control Flow

if/elsif/else, for, while, loop, given/when, unless, with/without, and repeat are supported.

```raku
for 1..5 -> $i { print "$i " }  # 1 2 3 4 5

given 42 {
    when 0..10   { say "small"  }
    when 11..100 { say "medium" }
    default      { say "big"    }
}
```

### Subs, Multi Dispatch, and Signatures

```raku
sub greet(Str $name) { say "Hello, $name!" }
greet("World");

multi sub fizz(Int $n where * %% 15) { "FizzBuzz" }
multi sub fizz(Int $n where * %% 3)  { "Fizz" }
multi sub fizz(Int $n where * %% 5)  { "Buzz" }
multi sub fizz(Int $n)               { $n }
say fizz(15);  # FizzBuzz
```

### Classes, Roles, and Inheritance

```raku
role Greetable {
    method greet() { say "Hello, I am {self.name}" }
}

class Person does Greetable {
    has $.name;
}

Person.new(name => "Alice").greet;  # Hello, I am Alice

class Animal { has $.name }
class Dog is Animal {
    method speak() { say "{self.name} says Woof!" }
}
Dog.new(name => "Rex").speak;       # Rex says Woof!
```

### Grammars and Regex

```raku
grammar CSV {
    token TOP  { <line>+ % "\n" }
    token line { <cell>+ % ","  }
    token cell { <-[,\n]>*      }
}
say CSV.parse("a,b,c").so;  # True

if "Hello123" ~~ /(\w+)(\d+)/ {
    say ~$0;  # Hello12
    say ~$1;  # 3
}
```

### Functional Programming

map, grep, reduce, sort, gather/take, sequences, and junctions.

```raku
say (1..10).grep(*.is-prime);           # (2 3 5 7)
say [+] 1..100;                         # 5050
say (1, 1, *+* ... *)[^10];            # (1 1 2 3 5 8 13 21 34 55)
say gather { for 1..10 { take $_ if $_ %% 3 } }.list;  # (3 6 9)
```

### Exception Handling

```raku
try {
    die "something went wrong";
    CATCH { default { say "Caught: {.message}" } }
}
```

### Promises

```raku
my $p = start { sleep 0.1; 42 };
say await $p;  # 42
```

### Enums and Subset Types

```raku
enum Color <Red Green Blue>;
say Red;            # Red
say Green.value;    # 1

subset Positive of Int where * > 0;
my Positive $x = 5;
```

### File I/O

```raku
spurt "output.txt", "Hello from mutsu!\n";
say slurp "output.txt";
```

### MAIN Sub for CLI Tools

Define a `MAIN` sub to get automatic argument parsing and usage messages:

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

### And More

- Phasers (BEGIN, INIT, ENTER, LEAVE, FIRST, NEXT, LAST, etc.)
- String methods (uc, lc, tc, split, join, chars, etc.)
- Module loading (`use`)
- Proc::Async for external processes
- Complex number arithmetic
- Set, Bag, Mix operations
- Junctions (any, all, one, none)

## Known Limitations

- **Single-threaded execution.** `start`/`await` work but run on a single thread.
- **Incomplete container semantics.** Some binding and container behaviors differ from Rakudo.
- **Limited exception types.** Not all `X::` exception classes are implemented.
- **No package manager integration.** There is no zef/ecosystem support; `use` works for local modules only.
- **Some advanced features are missing or incomplete.** Certain meta-programming, NativeCall, and supply/react patterns are not yet supported.

## Building

```bash
cargo build              # Debug build
cargo build --release    # Optimized build
make test                # Run local tests (prove t/)
make roast               # Run official Raku spec tests
```

## Requirements

- Rust 1.92.0+ (edition 2024)
- A C compiler (for pcre2-sys)

## Architecture

```
Source -> Parser (src/parser/) -> Compiler (src/compiler/) -> VM (src/vm/) -> Output
```

mutsu uses a bytecode VM architecture. Source code is parsed into an AST, compiled to bytecode (`OpCode` instructions), and executed by the VM. See [CLAUDE.md](CLAUDE.md) for detailed architecture documentation.

## Contributing

See [CLAUDE.md](CLAUDE.md) for development conventions, architecture details, and working agreements. See [PLAN.md](PLAN.md) for the project roadmap.

## Links

- [Raku documentation](https://docs.raku.org/)
- [Roast (official Raku test suite)](https://github.com/Raku/roast)
- [WebAssembly demo](https://tokuhirom.github.io/mutsu/)
