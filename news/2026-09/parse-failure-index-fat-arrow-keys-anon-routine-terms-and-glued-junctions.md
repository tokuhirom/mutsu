# Parse-failure index: qualified fat-arrow keys, anonymous-routine parameter terms, glued junctions and quoted brackets

Another batch off the `blocked_load` parse-failure index (#7954): four parse constructs and one
rendering gap. Three of them came from `PDF::Content`, the distribution whose six modules all
reported the same `PDF/Content/Ops.rakumod:248` and the largest single cluster left in the index;
the fourth from `Code::Coverable`, the next-largest (three distributions at one line).

## A `::`-qualified name followed by `=>` is not a pair key

Raku's `<fatarrow>` term rule keys on `<identifier>`, which admits no `::`. So `OpCode::A` is an
ordinary term and the `=>` after it is the ordinary infix, at its own very loose precedence — which
means a tighter infix on the left takes the *whole* left side as the pair's key:

```raku
enum OpCode <A B>;
say (OpCode::A | OpCode::B => 3).raku;
# raku:  any(OpCode::A, OpCode::B) => 3
# mutsu: any(OpCode::A, OpCode::B => 3)
```

mutsu claimed the arrow at term level for a qualified name too, binding it to the junction's right
operand instead of the junction. `PDF::Content` writes
`BEGIN %Store = ( OpCode::BeginText|OpCode::EndText => method {...}, ... )`, so the reading mutsu
took collapsed the whole initializer into an odd-element list.

The unqualified form is the opposite case and is unchanged: `A | B => 3` really is
`any(A, :B(3))` in raku too, because `B` *is* a simple identifier. Both readings now come from one
rule — `identifier_call`'s term-level arrow requires a simple identifier, and everything else falls
through to `expression`'s own fat-arrow handler, which already built the loose form (and already got
the positional-versus-named distinction right).

Pin: `t/lang/operators/fat-arrow-qualified-name-precedence.t`.

## An anonymous routine literal's parameters are in scope for its body

A sigilless parameter declares a TERM and a `&name` parameter declares a routine, so inside the body
a bare `m` IS that binding. A *named* `sub`/`method` declaration already registered both; the
anonymous `sub (...) { ... }` / `method (...) { ... }` literal did not, so its body was parsed
against what the name means outside it.

For a name that also spells a quote-like operator that is not a wrong lookup but a different lex.
`PDF::Content` has `method ( \c, \m, \y, \k) { @!FillColor = [ c, m, y, k ] }`; the body's `m` read
as a match, took `,` for its delimiter, and swallowed the signature's own `\y` as a regex escape, so
the error was `Unrecognized backslash sequence: \y` — pointing at a construct several lines up, and
naming a character that appears nowhere in the body. Four parameters were needed for it to fire at
all, because a two-parameter signature has no second comma for the match to close on.

`parse_block_body_routine_with_params` now does the registration, including for a destructuring
sub-signature, and opens a fresh parse generation for the same reason `block_with_pointy_params`
does: the memo is keyed by the input slice alone, so an entry made by a speculative pass over the
body — before these names existed — would otherwise be replayed at its old meaning.

Pin: `t/routines/signature/anon-routine-param-terms.t`.

## `&` glued to its left operand is the all-junction infix

A sigil opens a TERM, and a term cannot begin in the middle of a token, so `Int&Str` has no `&name`
reading at all. mutsu refused the infix whenever an identifier character followed the `&` — the
right guard for `f &g`, where the listop really does take `&g` as an argument, and far too wide
everywhere else. Every glued all-junction of two barewords was a hard parse error: `Int&Str`,
`A&B` over an enum's values, `f()&g()`. rakudo draws the line at exactly the same place:
`sub f($x) {…}; f &g` binds `&g`, while `f&g` is "Calling f() will never work with declared
signature ($x)", i.e. two operands of `&`.

`|` and `^` were never affected, because neither doubles as a sigil.

Pin: `t/lang/operators/infix-junction-glued-to-operand.t`.

## A nested Junction renders its eigenstates with `.raku`

Falling out of the first fix: a junction's `.raku` is its constructor call over the `.raku` of its
eigenstates, which held for a top-level junction — the interpreter renders that one itself — but not
for one nested in another value. The pure renderer had no `Junction` arm, so a nested junction fell
through to its string coercion and came back gisted, and a gisted enum eigenstate loses its
qualification: `((A|B) => 3).raku` gave `any(A, B) => 3` where raku gives
`any(OpCode::A, OpCode::B) => 3`. A Pair key is where it shows, which is exactly the shape the fix
above makes.

Pin: `t/types/junction-nested-raku-output.t`.

## A regex group body reads by one set of rules, whichever bracket opened it

`[ ... ]` and `( ... )` are the same construct with a different bracket, and the body in between
reads the same way: a backslash escape never moves the depth, a quoted string's content is literal,
`#` outside any `<...>` starts a comment, and a `<...>` is read by one of three sets of rules
depending on what it is. The `(...)` scanner had learned most of that, the last of it in the
previous batch; the `[...]` one still counted brackets raw, so a quoted `]` closed the group one
level down:

```raku
say (']x' ~~ /[[']']]/);
# raku:  ｢]｣
# mutsu: Unrecognized regex metacharacter ] (must be quoted to match literally)
```

One level of nesting happened to survive, because the truncated body still reached the matcher; two
did not. `Code::Coverable` writes
`elsif $line ~~ /^ \s* [[')' | ']'] \s+]? [is \s+ <[-_\w]>+ \s+]* '{' [\s+ '}']? $/`, so the
failure it reported was `Unexpected block in infix position` at the `while` fifteen lines *above* —
the enclosing construct, as usual for this index. `Code::Coverage` and `Test::Coverage` inherited it.

Both scanners are one function now (`scan_regex_group_body`), so neither can drift from the other
again — and unifying them forced the `<...>` rules to be stated properly, because the `(...)` scanner
had been getting two of the three kinds right by accident. A `<...>` is now one stack entry of a
known kind:

- a **character class** (`<[…]>`, `<-[…]>`, `<+[…]>`, `<:Letter>`) holds literal members, so neither
  a quote (`<-['"]>`) nor a group bracket (`<[.)]>`) means anything in it — and its own `[`/`]` are
  counted, so a `>` written as a member does not close the class. That last part only mattered once
  the class's brackets stopped reaching the group depth: `token verpart { ':ver<' $<v>=[<-[>]>+] '>' }`
  has a `>` member inside a `[…]` group, and the raw scanner it replaced got it right by letting the
  class's own brackets cancel out against the group's.
- a **lookaround** (`<before …>`, `<?after …>`, …) holds a nested regex, so the quote in
  `<!before '>}}'>` really does open a string.
- everything else — a word-list alternation (`< ! ' # >`), a named rule, a code assertion
  (`<!{ … }>`) — has no string syntax at all, so a quote in it is an ordinary character. The `(...)`
  scanner had been honouring quotes here too, which is only invisible because no group in the suite
  wrote a bare quote in a word list until `[…]` started sharing the code.

`Code::Coverable` now parses every module its META6 `provides` names.

Pin: `t/regex/syntax/regex-group-scan-quoted-bracket-alternation.t`.

## Where that leaves `PDF::Content`

`lib/PDF/Content/Ops.rakumod` now parses past line 248 and on to the end of the file; what it reports
instead is the `Function 'X::PDF::Content' needs parens to avoid gobbling block` shape that rakudo
emits verbatim for the same file when the dependency providing that exception class is absent, so it
is not a mutsu gap. One real gap is left in the distribution and was filed rather than forced
through: an *attributive parameter* in a plain `sub` nested in a method body
(`sub STORE($, $!text) { … }`, in `PDF/Content/Text/Box.rakumod`). mutsu rejects it at parse time
with `X::Syntax::NoSelf`, where rakudo accepts it — the nested sub closes over the method's invocant
— but simply relaxing the parse check turns the load failure into a silently wrong answer, because
the binder gives such a parameter a frame slot instead of writing the attribute. Filed as #8452.
