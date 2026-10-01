# Grammars built through the MOP: regex methods, `set_name`, and `~~ Grammar`

A grammar assembled at run time with `Metamodel::GrammarHOW.new_type` now works
the way rakudo's does (#10633):

```raku
my $g := Metamodel::GrammarHOW.new_type(:name<MyNewGrammar>);
my $x = EVAL 'regex { a | b }';
$g.^add_method('x', $x);
$x.set_name('x');
$g.^add_method('TOP', EVAL 'regex { <x> }');
$g.^compose;
say $g ~~ Grammar;   # True
say $g.parse('b');   # ｢b｣ x => ｢b｣
```

- `^add_method` with a regex value (an EVAL'd or literal `regex`/`token`/`rule`
  declarator term) installs it as the type's grammar rule, so `.parse` and a
  `<x>` subrule resolve it exactly like a rule declared in a `grammar` body.
- `^compose` gives a parentless `GrammarHOW` type the default `Grammar` parent,
  so it smartmatches `Grammar` and inherits `.parse`.
- `Code.set_name` works on a regex: the regex's code-object payload carries a
  name cell, renamed in place and seen through every alias (`Regex.name`).
  Anonymous declarator terms now always carry that payload.

This makes `Grammar::TokenProcessing`'s `t/06-rule-to-regex-conversion.rakutest`
pass. Per-evaluation identity of regex values (`===` between two evaluations of
the same literal) is still missing and is tracked in #10670.
