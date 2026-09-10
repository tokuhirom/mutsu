#!/usr/bin/env python3
"""Place every flat `t/*.t` file into its category directory.

The one-shot sweep for docs/t-directory-layout.md, kept in the tree so the
result is reproducible: re-running it must reproduce the committed layout
exactly, which is what makes a ~4000-file rename reviewable at all. Nobody has
to take the diff on trust -- run this and diff the plan against `git ls-files`.

    scripts/migrate-t-layout.py            # print the plan, and any unplaced files
    scripts/migrate-t-layout.py --apply    # git mv every file into place
    scripts/migrate-t-layout.py --check    # exit 1 if the tree disagrees with the plan

Placement is decided in two layers, in this order:

1. OVERRIDES -- an explicit basename -> category map. This is where a file goes
   when the rules below would place it wrongly. Judgment lives here, in one
   reviewable list, rather than being smeared through ever-more-specific regexes.
2. RULES -- ordered (category, regex) pairs, first match wins. The order matters:
   the more specific subject comes first, so `grammar` beats `regex` and
   `nativecall` beats `types`, per the document's tie-breakers.

A file matched by neither is reported and the run refuses to apply, so the tree
can never end up half-placed.
"""

import os
import re
import subprocess
import sys

# docs/t-directory-layout.md section 2. A closed set, mirrored in
# scripts/check-t-layout.sh.
CATEGORIES = [
    "collections", "concurrency", "control", "exceptions", "grammar", "io",
    "lang", "modules", "nativecall", "oo", "rakuast", "regex", "routines",
    "tooling", "types", "vm",
]

# First match wins. Patterns match the basename without its `.t`.
RULES = [
    # -- most specific subjects first ---------------------------------------
    ("rakuast", r"rakuast"),
    ("nativecall", r"nativecall|(^|-)native(-|$)|cstruct|carray|cpointer|cunion|cglobal|c-?array|repr-|is-native|nativecast|cbool|clong|csize|refresh-native"),
    ("grammar", r"grammar|(^|-)token(-|$)|(^|-)rule(-|$)|actions-|-actions|proto-?regex|parsefile|subparse|\bTOP\b"),
    ("concurrency", r"supply|supplier|promise|(^|-)react(-|$)|whenever|channel|thread|(^|-)lock(-|$)|atomic|scheduler|async|await|(^|-)cue(-|$)|proc-async|vow|kept-|broken-|start-block|racy|lock-async|semaphore|deadlock|concurren"),
    ("regex", r"regex|(^|-)rx(-|$)|subst|(^|-)tr(-|$)|trans-|smart-?match|(^|-)match(-|$)|match-|backtrack|lookahead|lookbehind|named-capture|charclass|char-class|quantifier|ratchet|anchor|(^|-)comb(-|$)|capture-marker|ws-rule|regexp|-m-|^m-|sequential-alt|conjunction"),

    # -- language machinery --------------------------------------------------
    ("rakuast", r"^ast-|-ast$"),
    ("tooling", r"^cli-|dump-ast|^lsp|^mzef|^zef-|repl|command-line|shebang|^main-|^exit|^usage|^version-flag|^help-|^test-util|^is-run|^is_run|^subtest|^todo-|^plan-|^bail|^diag|^flunk|^pass-|test-module|^tap-|^prove"),
    ("modules", r"^module|^use-|-use$|^need-|^import|export|^lib-|precomp|^unit-|(^|-)package(-|$)|^require|batteries|^install|distribution|^depends|compunit|^bundled|^mixin-module|-module(-|$)|^our-|^globalish"),
    ("nativecall", r"^libc-|^ffi-"),

    # -- errors --------------------------------------------------------------
    ("exceptions", r"exception|^die-|^fail|(^|-)catch(-|$)|throw|^warn|backtrace|^x-|control-exception|^resume|(^|-)try(-|$)|^cannot-|-error(-|$)|^error-|sorry|^undefined-|^typed-error"),

    # -- io ------------------------------------------------------------------
    ("io", r"(^|-)io(-|$)|socket|spurt|slurp|^file-|^path-|^dir-|(^|-)handle(-|$)|^proc(-|$)|^proc-|stdin|stdout|stderr|tempfile|temp-dir|^open-|^close-|^chdir|^chmod|^mkdir|^unlink|^rename-|^copy-|cathandle|^printf-to|^say-to|^note-|^print-to|^lines-|^getc|^seek|^eof"),

    # -- oo ------------------------------------------------------------------
    ("oo", r"^class|(^|-)role(-|$)|role-|(^|-)method(-|$)|method-|^attr|attribute|(^|-)trait(-|$)|trait-|(^|-)mro(-|$)|mixin|(^|-)does(-|$)|does-|inherit|submethod|bless|metamodel|^mop-|introspect|^how-|accessor|^new-|construct|^self-|^private-|^parent|^also-|^add-method|composition|punn|^augment|^anon-class|^subset-|^enum-class|^twigil-attr|^build|^tweak|^clone-|^wrap-method|^multi-method|^dispatch-method|^can-|^isa-|^what-|^callsame-method|^protected|^abstract|^stub|^delegat|handles"),

    # -- routines ------------------------------------------------------------
    ("routines", r"^sub-|(^|-)signature(-|$)|signature-|param|(^|-)proto(-|$)|proto-|multi|closure|(^|-)wrap(-|$)|wrap-|^return|capture|^arg-|dispatch|candidate|slurpy|placeholder|^anon-|callable|^call-|currying|^assuming|nested-sub|routine|^named-arg|^pointy|^lambda|^block-param|^callsame|^nextsame|^callwith|^nextwith|^samewith|^lexical-sub|^recurs|^tail-|^curry|^fatarrow-arg|^whatever-curry|^rw-param|^copy-param|^optional-|^required-|^default-arg"),

    # -- data ----------------------------------------------------------------
    ("collections", r"^array|^hash|^list|(^|-)seq(-|$)|seq-|^set-|^bag|^mix-|^range|^pair|slice|subscript|iterat|lazy|^map-|^grep|^sort|reduce|^zip|^cross|^push|^pop|^shift|^unshift|^splice|^kv-|^keys|^values|element|^flat|^gather|^take|^rotor|^classify|^categorize|^head-|^tail|^first-|^elems|^join-|^reverse|^unique|^squish|^produce|^antipairs|^invert|^append|^prepend|^end-of|^index-|^deepmap|^duckmap|^nodemap|^batch|^combinations|^permutations|^roll|^pick|^min-|^max-|^sum|^postcircumfix|^zen-slice|^multidim|^shaped|^nested-array|^nested-hash|^itemiz|^listop|^infinite-list|^whatever-slice"),

    # -- control -------------------------------------------------------------
    ("control", r"^for-|^while|^loop|^given|^when|^if-|^unless|^repeat|phaser|^last-|^next-|^redo|^do-|^begin-|^end-|^enter-|^leave-|^init-|^check-|topic|^ternary|^with-|^without|^succeed|^proceed|^default-block|^statement-mod|^label|^goto|^sink|^once-|^quietly|^eager-|^hyper-stmt|^race-stmt|^nested-loop|^bare-block|^block-"),

    # -- types ---------------------------------------------------------------
    ("types", r"^int-|^num-|^rat-|^str-|^buf-|^bool-|(^|-)enum(-|$)|enum-|subset|coercion|coerce|allomorph|typed|^type-|numeric|^complex|^version|^date|^junction|^nil-|^mu-|^any-|^cool|^blob|^uni|stringif|^gist|^raku-method|^perl-method|^bigint|^bigrat|^fatrat|^number|^numify|^stringy|^smiley|^definedness|^defined-|^instant|^duration|^ord-|^chr-|^base-|^radix|^sprintf|^printf|^fmt|^round|^floor|^ceiling|^truncate|^abs-|^sign|^log-|^exp-|^sqrt|^rand|^srand|^prime|^gcd|^lcm|^div-|^mod-|^pow|^bit-|^bitwise|^shift-large|^inf-|^nan-|^epsilon|^approx|^collat|^unicode|^nfc|^nfd|^nfk|^grapheme|^codepoint|^encode|^decode|^utf|^ascii|^latin|^chars-|^substr|^index|^rindex|^split-|^words|^trim|^lc-|^uc-|^tc-|^wordcase|^flip-|^chop|^chomp|^sprintf-|^base64|^whatever(-|$)|^hyperwhatever|^callable-type|^set-type"),

    # -- vm internals --------------------------------------------------------
    ("vm", r"writeback|coherence|(^|-)env(-|$)|env-|jit|^gc-|opcode|^slot-|container|^adr[0-9]|dual-store|^vm-|bytecode|^frame-|(^|-)scope(-|$)|scope-|lexical|^state-|^let-|^temp-|^bind-|^assign|^var-|^decl|^name-|^lvalue|^rw-|^my-|^our-decl|^dynamic|^context-|^caller|^callframe|^callsite|^compile-time|^compiler|^constant-fold|^inline|^optimi|^perf-|^bench|^memory|^leak|^recursion-depth|^stack-|^identity|^which|^clone-preserve|^cas(-|$)|^shared-var|^magic-var|^special-var|^sigilless"),

    # -- surface syntax, last: it is the catch-all for "how it is written" ---
    ("lang", r"syntax|operator|^op-|quot|heredoc|interpolat|precedence|^term-|sigil|^string-|^literal|^comment|^whitespace|^pod-|^infix|^prefix|^postfix|^circumfix|^meta|hyper|^chained|^adverb|^colonpair|^angle|^bracket|^paren|^semicolon|^comma|^backslash|^escape|^unicode-op|^custom-op|^reduction|^feed|^sequence-op|^smart|^chain|^ws-|^bare-|^bom-|^encoding-decl|^q-|^qq|^qw|^words-quote|^long-dot|^dotty|^postcircum"),

    # -- second pass -------------------------------------------------------
    # Appended AFTER the block above on purpose: these only ever see files no
    # earlier rule claimed, so adding one can never move a file that was
    # already placed.
    ("modules", r"-battery$|^json|rakudo-internals-json|^repo-|^cur-install|^no-pragma|^no-worries|^language-version|^core-skip-list"),
    ("tooling", r"^test-|^testing-|^is-deeply|^lives-ok|^cmp-ok|^skip-list|^doc-|^prompt-|^rakuseen|^fudge-|^legacy-body-drop|^snitch"),
    ("vm", r"^const|^gate-b-|^bound-|^pseudo|^stash-|-stash$|^symbolic-deref|^source-line|^source-literals|^scalar-|^proxy-|^shadow-slot|^undefine-shadow|^scoped-|^locals-frame|^process-|^nqp-|^phantom-entry|^deferred-|^dualstore|^rebound-|^hot-path|^mark-context|^redeclar|^outer-redeclaration|^per-class-same-name|^sibling-block|^varref-|^raw-|^generic-bind|^create-slots|^panic-recovery|^light-call|^mixed-light-call|^named-light-call|^positional-light|^one-pass-parsing|^known-type-constraint|^conditional-my|^grouped-declaration|^has-decl|^has-attr|^indirect-|^non-variable-dollar|^underscore-kebab|^expr-decl|^strict-|^virtual-call-attr|^walk-orderings|^start-dynamic|^start-panic|^start-self|^protect-block|^shared-|^scalar|^immutable-lvalue|^destroy"),
    ("collections", r"^at-pos|^item-|^slip-|^quanthash|^setbagmix|^object-hash|^tied-hash|^nested-assoc|^nested-autoviv|^nested-whatever-index|^tree-itemization|^iterable-instance|^is-list-subclass|^is-copy-array|^positional-index|^positional-read|^out-of-range-scalar-index|^computed-index|^incdec-|^elem-index|^count-only|^negated-set-op|^minmax|^polymod|^pack-unpack|^group-of|^nth-whatevercode|^finite-|^immutable-list|^subbuf|^shape-illegal|^say-slip"),
    ("routines", r"^destructure-|^subsig-|^named-array-destructure|^nested-pair-subsignature|^where-|^is-default|^is-rw-traits|^whatevercode|^invocant-marker|^lastcall|^too-many-positionals|^missing-block-sub|^named-sub-literal|^keyword-|^standalone-str-funcs|^more-functions|^more-methods|^misc-builtins|^conversion-functions|^os-functions|^special-form-override|^overloading-fallbacks|^known-call-ternary|^statement-call-sinks|^sleep-listop|^deferred-map"),
    ("types", r"^typecheck-|^format|^instance-gist|^objat-gist|^say-gist|^succ-pred|^temporal-|^titlecase|^samecase|^samemark|^fc-|^contains-ignoremark|^spaceship|^float-num|^decimal-|^big|^divisible-|^divide-by-zero|^empty-string-numifies|^concat-|^raku-string-escape|^definite-|^dd-instance|^nominalize|^object-type-reprs|^builtin-subclass|^user-which-identity|^qualified-mu-new|^cyclic-instance|^roots-are-complex|^geometric-sequence|^windows-125|^streaming-decoder|^encoding-decoder|^source-literals|^rat$|^sequence$|^set$|^trans$|^map$|^indent$"),
    ("exceptions", r"^comp-group-|^did-you-mean|^no-such-symbol|^undeclared-|^weird-errors|^vcs-conflict-marker|^obsolete-|^duplicated-prefix|^malformed-|^two-terms-in-a-row|^p5-foreach|^perl5var|^trusts-undeclared|^suppressed-type|^extension-null|^get-out|^bug-coverage|^doesnt-warn|^control(-|$)|^resumable-control"),
    ("io", r"^iopath-|^iospec-|^kernel-cpu-cores|^platform-library-name|^custom-out-print"),
    ("control", r"^keep-undo|^named-alias-and-loop|^modifier-cond|^double-statement-modifier|^line-ending-block|^stmt-terminator|^trailing-comma-before-statement|^statement-level-begin|^emit-done-controlflow|^orwith|^done-paren"),
    ("oo", r"^namespaced-class|^nested-class-short-name|^nested-instance-raku|^roles-|^route-block-dsl|^methods-instance|^user-class-shadows|^user-type-shadows|^unmarshal-mop|^ctor-|^qualified-name|^user-group-dynamic|^variable-custom-traits|^variable-traits|^forward-declaration|^abstract|^issue-777|^issue-778"),
    ("lang", r"^amp-|^andthen|^bareword-|^colon-|^compound-assign|^computed-declarator|^dot|^eval-|^interp-|^parser-|^slang-|^exec-call|^fat-arrow|^negated-pair|^nonassoc|^ordered-alternation|^short-circuit-compound|^word-compound-assign|^xx-|^zprintf|^user-infix|^user-postcircumfix|^diffy-assign|^elem-index-meta|^embedded-qqw|^exists-delete-adverb|^value-dynamic-adverb|^long-dot|^digit-var|^code-var|^code-line|^concat|^item-deref|^deref-bind|^hyphenated-|^issue-77|^tolerance-dynamic|^skip|^fatal-mode|^uri-query|^http-deps"),
]

# Basename (without `.t`) -> category. Judgment calls and rule mistakes.
# Keep sorted; every entry should be obvious from the file's subject.
OVERRIDES: dict[str, str] = {
    # Type-matching against a core type name that a lexical shadows: the
    # question is which type the matcher resolves, not the shadowing itself.
    "core-type-not-shadowed-in-typematch": "types",
    # `where` on a generic type parameter -- a signature constraint.
    "generic-where": "routines",
    # Whether a generic class nominalizes -- a type-system property.
    "generics-nominalizable-class": "types",
    # `is-eqv` compares values structurally: an equivalence-semantics test.
    "is-eqv": "types",
    # A `my $x = $x` style alias must not leak its name outward: scoping.
    "nested-alias-name-no-leak": "vm",
    # A nested `Any` type constraint on a parameter.
    "nested-any-type-constraint": "routines",
}


# Second level, for the categories that would otherwise be too big to scan.
# docs/t-directory-layout.md section 2 puts the soft cap at ~200 files; roast's
# own largest directory is 70. Applied only within the named category, first
# match wins; a file matching nothing stays directly in the category. Every
# subcategory here must also be listed in scripts/check-t-layout.sh.
SUBRULES: dict[str, list[tuple[str, str]]] = {
    "collections": [
        ("set-bag-mix", r"^set|^bag|^mix|quanthash|setbagmix|^negated-set"),
        ("range-pair", r"^range|^pair|^negated-pair|^fatarrow|^antipairs"),
        ("subscript", r"slice|subscript|^at-pos|^index-|^incdec-|postcircumfix|^computed-index|^positional-index|^out-of-range|^nested-whatever-index|^elem-index|^shape-illegal|^multidim|^shaped|^zen-"),
        ("hash", r"^hash|^object-hash|^tied-hash|^nested-assoc|^nested-autoviv|^kv-|^keys|^values|assoc"),
        ("array", r"^array|^push|^pop|^shift|^unshift|^splice|^append|^prepend|^nested-array|^is-copy-array|^immutable-list"),
        ("transform", r"^map|^grep|^sort|reduce|^zip|^cross|^rotor|^classify|^categorize|^produce|^unique|^squish|^reverse|^join|^first-|^head-|^tail|^min-|^max-|^sum|^roll|^pick|^combinations|^permutations|^deepmap|^duckmap|^nodemap|^batch|^polymod|^pack-unpack|^invert|^flat"),
        ("lazy-seq", r"^seq|seq-|lazy|iterat|^gather|^take|^finite-|^infinite|^tree-itemization|^slip|^item-"),
    ],
    "oo": [
        ("role", r"role"),
        ("attribute", r"^attr|attribute|accessor|^has-attr|^per-class-same-name"),
        ("trait", r"trait|handles|^delegat"),
        ("mop", r"metamodel|^mop-|introspect|^how-|^can-|^isa-|^what-|nominaliz|^object-type-reprs|^unmarshal-mop|archetype"),
        ("construct", r"^new-|construct|bless|^build|^tweak|^ctor-|^clone-"),
        ("method", r"method"),
        ("class", r"class|^augment|^parent|^also-|inherit|(^|-)mro(-|$)|composition|submethod|^namespaced|^qualified-name"),
    ],
    "regex": [
        ("subst", r"subst|(^|-)tr(-|$)|trans-|samecase|samemark"),
        ("match", r"(^|-)match(-|$)|match-|capture|^comb|smart-?match"),
        ("syntax", r"quantifier|charclass|char-class|anchor|backtrack|lookahead|lookbehind|ratchet|alternation|conjunction|^ws-|sequential-alt"),
    ],
    "types": [
        ("numeric", r"^int-|^num-|^rat|^complex|^big|^numeric|^numify|^float|^decimal|^polymod|^divis|^divide-by-zero|^round|^floor|^ceiling|^truncate|^abs-|^sign|^log-|^exp-|^sqrt|^rand|^srand|^prime|^gcd|^lcm|^div-|^mod-|^pow|^bit|^radix|^base-|^inf-|^nan-|^epsilon|^approx|^spaceship|^geometric|^roots-are-complex|^nonassoc-comparison"),
        ("string", r"^str|^chars-|^substr|^split-|^words|^trim|^lc-|^uc-|^tc-|^wordcase|^flip-|^chop|^chomp|^sprintf|^printf|^fmt|^zprintf|^format|^concat|^titlecase|^samecase|^samemark|^fc-|^contains|^encode|^decode|^utf|^ascii|^latin|^windows-125|^unicode|^nfc|^nfd|^nfk|^grapheme|^codepoint|^collat|^ord-|^chr-|^uni|^raku-string-escape|^source-literals|^streaming-decoder|^encoding-decoder|^empty-string"),
        ("enum-subset", r"enum|subset|^definite|^smiley|^nominaliz|^generics-nominalizable"),
        ("coercion", r"coerc|allomorph|^typecheck|^typed|^type-|stringif|^gist|^instance-gist|^objat-gist|^say-gist|^core-type-not-shadowed|^is-eqv|^builtin-subclass|^object-type-reprs|^user-which-identity|^cyclic-instance|^dd-instance|^qualified-mu-new"),
        ("temporal", r"^date|^temporal|^instant|^duration|^version"),
    ],
    "vm": [
        ("writeback", r"writeback|coherence|dual-store|dualstore|^shared-|^scalar-|^proxy-|^container|^cas(-|$)|^immutable-lvalue|^raw-"),
        ("scope", r"(^|-)scope(-|$)|scope-|lexical|^shadow|^undefine-shadow|^pseudo|^stash|-stash$|^outer-redeclaration|^redeclar|^sibling-block|^nested-alias|^my-|^our-|^state-|^let-|^temp-|^dynamic|^context-|^process-|^conditional-my|^strict-"),
        ("binding", r"^bind-|^bound-|^assign|^var-|^decl|^lvalue|^rw-|^varref|^generic-bind|^symbolic-deref|^deref-bind|^phantom-entry|^const|^has-decl|^has-attr|^expr-decl|^grouped-declaration|^indirect-"),
        ("frames", r"^frame-|^caller|^callframe|^callsite|^locals-frame|^source-line|^stack-|^panic-recovery|^recursion-depth|^deferred-|^mark-context|^light-call|^mixed-light-call|^named-light-call|^positional-light"),
        ("codegen", r"jit|^gc-|opcode|bytecode|^slot-|^compile-time|^compiler|^constant-fold|^inline|^optimi|^perf-|^bench|^memory|^leak|^hot-path|^one-pass-parsing|^adr[0-9]|^vm-|^create-slots|^legacy-body-drop|^rebound-"),
    ],
    "concurrency": [
        ("supply", r"supply|supplier|whenever|(^|-)react(-|$)|^emit|^tap"),
        ("promise", r"promise|^await|^start|^vow|^kept|^broken"),
        ("thread-lock", r"thread|(^|-)lock(-|$)|atomic|semaphore|deadlock|^protect-block|scheduler|(^|-)cue(-|$)|channel"),
    ],
    "lang": [
        ("operators", r"operator|^op-|^infix|^prefix|^postfix|^circumfix|^meta|hyper|^chained|^chain|^reduction|^user-infix|^user-postcircumfix|^nonassoc|^diffy|^amp-|^andthen|^orwith|^xx-|^spaceship|^feed|^sequence-op|^short-circuit|^compound-assign|^word-compound-assign|^dot|^elem-index-meta|^negated-pair|^fat-arrow"),
        ("quoting", r"quot|heredoc|interpolat|^q-|^qq|^qw|^words-quote|^embedded-qqw|^escape|^backslash|^raku-string-escape|^interp-"),
        ("adverbs", r"^adverb|^colonpair|^colon-|^exists-delete-adverb|^value-dynamic-adverb|^computed-declarator"),
        ("parsing", r"^parser-|^slang-|^syntax|^whitespace|^comment|^semicolon|^comma|^bracket|^paren|^ws-|^bareword|^bare-|^bom-|^long-dot|^two-terms|^one-pass"),
    ],
    "modules": [
        ("batteries", r"-battery$|batteries|^json|^http-deps|^uri-query"),
        ("compunit", r"compunit|precomp|^repo-|^cur-install|^install|distribution|^depends|^lib-"),
        ("import-export", r"export|^import|^use-|-use$|^need-|^require|^unit-"),
    ],
    "routines": [
        ("signature", r"param|signature|slurpy|destructure|subsig|^where-|^is-default|^is-rw-traits|^invocant-marker|^optional-|^required-|^default-arg|^named-arg|^too-many-positionals|^nested-any-type-constraint"),
        ("dispatch", r"multi|(^|-)proto(-|$)|proto-|dispatch|candidate|^callsame|^nextsame|^callwith|^nextwith|^samewith|^lastcall|^overloading-fallbacks|^special-form-override"),
        ("closure", r"closure|^wrap|wrap-|^assuming|^curry|currying|^whatevercode|^lambda|^pointy|^anon-"),
        ("call", r"^call-|^light-call|^mixed-light-call|^named-light-call|^positional-light|capture|^arg-|^return|^recurs|^tail-|^statement-call-sinks|^sleep-listop|^deferred-map|^known-call-ternary"),
    ],
}


def category_for(stem: str) -> str | None:
    """The directory under `t/` this file belongs in, `cat` or `cat/subcat`."""
    if stem in OVERRIDES:
        cat = OVERRIDES[stem]
    else:
        cat = next((c for c, pat in RULES if re.search(pat, stem)), None)
        if cat is None:
            return None
    for subcat, pat in SUBRULES.get(cat, []):
        if re.search(pat, stem):
            return f"{cat}/{subcat}"
    return cat


def plan(root: str) -> tuple[list[tuple[str, str]], list[str]]:
    tdir = os.path.join(root, "t")
    moves, unplaced = [], []
    for name in sorted(os.listdir(tdir)):
        if not name.endswith(".t"):
            continue
        if not os.path.isfile(os.path.join(tdir, name)):
            continue
        stem = name[:-2]
        cat = category_for(stem)
        if cat is None:
            unplaced.append(name)
        else:
            moves.append((f"t/{name}", f"t/{cat}/{name}"))
    return moves, unplaced


def main() -> int:
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    os.chdir(root)
    apply = "--apply" in sys.argv
    check = "--check" in sys.argv

    moves, unplaced = plan(root)

    if unplaced:
        print(f"{len(unplaced)} file(s) match no rule and have no override:", file=sys.stderr)
        for n in unplaced:
            print(f"    t/{n}", file=sys.stderr)
        print("Add a RULE or an OVERRIDES entry for each before applying.", file=sys.stderr)
        return 1

    counts: dict[str, int] = {}
    for _, dst in moves:
        d = "/".join(dst.split("/")[1:-1])
        counts[d] = counts.get(d, 0) + 1

    if check:
        # Verify the tree ON DISK against the rules, not the (now empty) list of
        # pending moves -- after the migration there is nothing left to plan, so
        # checking the plan would vacuously pass. This is the invariant worth
        # holding: every placed file is where the rules say it goes, which is
        # what makes the layout reproducible rather than a one-off sort.
        wrong, unruled = [], []
        for dirpath, _, names in os.walk("t"):
            for name in names:
                if not name.endswith(".t"):
                    continue
                actual = os.path.relpath(dirpath, "t")
                want = category_for(name[:-2])
                if want is None:
                    unruled.append(os.path.join(dirpath, name))
                elif want != actual:
                    wrong.append((os.path.join(dirpath, name), f"t/{want}/{name}"))
        for path in sorted(unruled):
            print(f"{path}: matches no rule and has no override", file=sys.stderr)
        for path, want in sorted(wrong):
            print(f"{path}: rules place this at {want}", file=sys.stderr)
        if wrong or unruled:
            print(
                f"{len(wrong) + len(unruled)} file(s) disagree with the rules. Either move the "
                "file, or add a RULE/OVERRIDES entry that puts it where it is.",
                file=sys.stderr,
            )
            return 1
        total = sum(1 for _, _, ns in os.walk("t") for n in ns if n.endswith(".t"))
        print(f"t/ layout matches the rules ({total} files)")
        return 0

    if not apply:
        for cat in sorted(counts):
            print(f"{counts[cat]:5d}  t/{cat}/")
        print(f"{len(moves):5d}  total")
        return 0

    for _, dst in moves:
        os.makedirs(os.path.dirname(dst), exist_ok=True)
    # One `git mv` per batch keeps the command line sane and the rename
    # detection intact.
    BATCH = 200
    for i in range(0, len(moves), BATCH):
        chunk = moves[i:i + BATCH]
        for src, dst in chunk:
            subprocess.run(["git", "mv", src, dst], check=True)
    print(f"moved {len(moves)} files")
    return 0


if __name__ == "__main__":
    sys.exit(main())
