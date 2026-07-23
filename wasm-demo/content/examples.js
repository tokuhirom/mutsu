/**
 * Ready-made code for the two hands-on pages.
 *
 * `PROGRAMS` are whole programs for the playground: they print things, and each
 * one runs on its own.  `REPL_LINES` are single expressions for the REPL, where
 * the point is the value a line evaluates to and what the next line can do with
 * it — so they are deliberately short and build on each other.
 *
 * Code is shared between languages, prose is not (same rule as the corpora):
 * `desc` carries one string per language.
 */

export const PROGRAMS = [
  {
    group: { en: 'Basics', ja: '基本' },
    name: 'Hello',
    desc: { en: 'The obligatory greeting.', ja: 'お約束の挨拶。' },
    code: `say "Hello, World!";`,
  },
  {
    group: { en: 'Basics', ja: '基本' },
    name: 'FizzBuzz',
    desc: {
      en: '%% is the "divisible by" operator — no modulo-equals-zero dance.',
      ja: '%% は「割り切れる」演算子。剰余がゼロかを書く必要はありません。',
    },
    code: `for 1..20 -> $n {
    if    $n %% 15 { say "FizzBuzz" }
    elsif $n %% 3  { say "Fizz" }
    elsif $n %% 5  { say "Buzz" }
    else           { say $n }
}`,
  },
  {
    group: { en: 'Basics', ja: '基本' },
    name: 'Rationals',
    desc: {
      en: 'Raku divides into exact Rats, so 0.1 + 0.2 == 0.3 really is True.',
      ja: '割り算は正確な Rat になるので、0.1 + 0.2 == 0.3 は本当に True です。',
    },
    code: `say 0.1 + 0.2 == 0.3;      # True - not a Num!
my $third = 1 / 3;
say $third;                # 0.333333
say $third.nude;           # (1 3) - numerator and denominator
say $third * 3 == 1;       # True
say 2 ** 100;              # arbitrary precision Int`,
  },
  {
    group: { en: 'Basics', ja: '基本' },
    name: 'Ranges',
    desc: {
      en: 'Ranges are first-class and work on strings too.',
      ja: 'Range は第一級の値で、文字列にも使えます。',
    },
    code: `say (1..10).sum;
say ('a'..'e').join('-');
say (1..*).head(5);        # infinite range, lazily taken
say (1..10).grep(*.is-prime);`,
  },

  {
    group: { en: 'Functional', ja: '関数型' },
    name: 'Map & Grep',
    desc: {
      en: '* builds a lambda: * ** 2 is "square it".',
      ja: '* はラムダを作ります。* ** 2 は「2 乗する」。',
    },
    code: `my @nums = 1..10;
my @evens = @nums.grep(* %% 2);
my @squares = @evens.map(* ** 2);
say "Evens:   @evens[]";
say "Squares: @squares[]";`,
  },
  {
    group: { en: 'Functional', ja: '関数型' },
    name: 'Sequences',
    desc: {
      en: 'The ... operator infers the rule and produces a lazy sequence.',
      ja: '... 演算子は規則を推測して遅延シーケンスを作ります。',
    },
    code: `my @fib = 0, 1, * + * ... *;
say @fib[^10];

say (1, 2, 4 ... 64);      # geometric, inferred
say ('a' ... 'j').join;`,
  },
  {
    group: { en: 'Functional', ja: '関数型' },
    name: 'Reduce & Hyper',
    desc: {
      en: '[+] reduces with +; >>.<< applies a method element-wise.',
      ja: '[+] は + での畳み込み、>>.<< はメソッドの要素ごと適用です。',
    },
    code: `say [+] 1..10;             # 55
say [*] 1..5;              # 120
say [~] <hello world>;
say [max] 3, 9, 2;

my @a = 1, 2, 3;
say @a >>*>> 10;           # hyper: (10 20 30)
say (-5, 3, -1)>>.abs;`,
  },
  {
    group: { en: 'Functional', ja: '関数型' },
    name: 'gather/take',
    desc: {
      en: 'gather/take is a coroutine-ish lazy list producer.',
      ja: 'gather/take はコルーチン風の遅延リスト生成です。',
    },
    code: `my @collatz = gather {
    my $n = 27;
    while $n > 1 {
        take $n;
        $n = $n %% 2 ?? $n div 2 !! 3 * $n + 1;
    }
    take 1;
}
say @collatz.elems;
say @collatz.head(10);
say @collatz.max;`,
  },
  {
    group: { en: 'Functional', ja: '関数型' },
    name: 'Junctions',
    desc: {
      en: 'One value that is several values at once — comparisons thread over it.',
      ja: '複数の値を同時に持つ 1 つの値。比較はその全体に対して行われます。',
    },
    code: `my $x = 5;
say "in the set"   if $x == any(3, 5, 9);
say "all positive" if all(1, 2, $x) > 0;
say "none is even" if none(1, 3, $x) %% 2;
say so 5 == 3 | 5 | 9;     # | is the any-junction operator`,
  },
  {
    group: { en: 'Functional', ja: '関数型' },
    name: 'Sets & Bags',
    desc: {
      en: 'Set operators use real math symbols (ASCII alternatives exist).',
      ja: '集合演算子は本物の数学記号です（ASCII の別名もあります）。',
    },
    code: `my $a = set <apple banana cherry>;
my $b = set <banana cherry durian>;
say $a (&) $b;             # intersection
say $a (|) $b;             # union
say $a (-) $b;             # difference
say 'apple' (elem) $a;

my $bag = bag <a b a c a b>;
say $bag<a>;               # 3
say $bag.sort(-*.value).head(2).map(*.key);`,
  },

  {
    group: { en: 'Objects', ja: 'オブジェクト' },
    name: 'Classes',
    desc: {
      en: 'has $.x gives a public accessor; has $!x stays private.',
      ja: 'has $.x は公開アクセサ付き、has $!x は非公開のままです。',
    },
    code: `class Point {
    has $.x is required;
    has $.y is required;
    method distance { sqrt($.x ** 2 + $.y ** 2) }
    method Str { "($.x, $.y)" }
}
my $p = Point.new(x => 3, y => 4);
say $p;                    # .gist - the default rendering
say ~$p;                   # our Str method
say $p.distance;`,
  },
  {
    group: { en: 'Objects', ja: 'オブジェクト' },
    name: 'Roles',
    desc: {
      en: 'Roles compose behaviour into classes — flat, not inherited.',
      ja: 'Role は振る舞いをクラスに合成します。継承ではなくフラットな合成です。',
    },
    code: `role Greet {
    method greet { "Hi, I am { self.name }" }
}
class Dog does Greet {
    has $.name;
    method speak { "Woof!" }
}
my $d = Dog.new(name => 'Rex');
say $d.greet;
say $d.speak;
say $d ~~ Greet;`,
  },
  {
    group: { en: 'Objects', ja: 'オブジェクト' },
    name: 'Multi dispatch',
    desc: {
      en: 'Dispatch on type, arity, and even on a where-constraint.',
      ja: '型・引数の数・where 制約でディスパッチできます。',
    },
    code: `multi fact(0) { 1 }
multi fact(Int $n where * > 0) { $n * fact($n - 1) }
say fact(10);

multi describe(Int $n) { "an Int: $n" }
multi describe(Str $s) { "a Str: $s" }
multi describe(@a)     { "a list of {@a.elems}" }
say describe($_) for 42, "hi", [1,2,3];`,
  },
  {
    group: { en: 'Objects', ja: 'オブジェクト' },
    name: 'Subsets',
    desc: {
      en: 'A named type plus a predicate — checked at binding time.',
      ja: '名前付きの型と述語の組。束縛時に検査されます。',
    },
    code: `subset Even of Int where * %% 2;
sub halve(Even $n) { $n div 2 }
say halve(10);

my $err = try { halve(7) };
say $!.^name if $!;`,
  },

  {
    group: { en: 'Text', ja: 'テキスト' },
    name: 'Regex',
    desc: {
      en: 'Raku regexes: whitespace is free, <[...]> is a character class.',
      ja: 'Raku の正規表現では空白は自由で、文字クラスは <[...]> と書きます。',
    },
    code: `my $str = "Hello World 2026";
if $str ~~ / (<[A..Z]>\\w+) \\s (\\w+) \\s (\\d ** 4) / {
    say "matched: $/";
    say "first:   $0";
    say "year:    $2";
}
say "a1b2c3".comb(/\\d/).join(',');
say "2026-07-22".split('-').map(+*);`,
  },
  {
    group: { en: 'Text', ja: 'テキスト' },
    name: 'Named rules',
    desc: {
      en: 'Regexes can be named and nested, which is how grammars are built.',
      ja: '正規表現には名前を付けて入れ子にできます。Grammar はこの延長線上です。',
    },
    code: `my regex ident { <[a..zA..Z_]> \\w* }
my regex num   { \\d+ }
if "count = 42" ~~ / <ident> \\s* '=' \\s* <num> / {
    say "name  = ", ~$<ident>;
    say "value = ", +$<num>;
}`,
  },
  {
    group: { en: 'Text', ja: 'テキスト' },
    name: 'Substitution',
    desc: {
      en: '.subst returns a new string; :g goes global.',
      ja: '.subst は新しい文字列を返します。:g で全体に適用されます。',
    },
    code: `my $s = "the quick brown fox";
say $s.subst('quick', 'slow');
say $s.subst(/\\w+/, *.tc, :g);
say $s.trans('aeiou' => 'AEIOU');
say $s.words.map(*.tc).join(' ');`,
  },
  {
    group: { en: 'Text', ja: 'テキスト' },
    name: 'Strings',
    desc: {
      en: 'Interpolation runs arbitrary code inside { }.',
      ja: '補間の { } の中では任意のコードが動きます。',
    },
    code: `my @xs = 1..5;
say "sum of @xs[] is { @xs.sum }";
say "unicode: \\c[SNOWMAN] { "snow" x 3 }";
say "raku".flip, " ", "abc".uc, " ", "Hello".chars;
say "%.3f".sprintf(pi);`,
  },
];

/**
 * One-liners for the REPL, in the order they are meant to be typed: the later
 * ones use what the earlier ones declared, which is the whole reason a session
 * exists.
 */
export const REPL_LINES = [
  '1 + 2 * 3',
  '(1..10).sum',
  'my @fib = 0, 1, * + * ... *',
  '@fib[^12]',
  'my %ages = Alice => 30, Bob => 25',
  '%ages.sort(*.value)».key',
  'sub double($n) { $n * 2 }',
  '(1..5).map(&double)',
  'class P { has $.x; method twice { $!x * 2 } }',
  'P.new(x => 21).twice',
  '"the quick brown fox".words».tc.join(" ")',
  '"2026-07-23" ~~ /(\\d+) "-" (\\d+) "-" (\\d+)/',
  '<a b c> (&) <b c d>',
  '[1, 2, 3] Z+ [10, 20, 30]',
  '"🇯🇵 raku".chars',
];
