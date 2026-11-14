grammar Mutsu::Slang::Host::Base {
    token end-keyword { » <!before \h* '=>'> }
    # HLL::Grammar's `panic`: a compile-time error carrying the message.
    method panic(*@args) { X::AdHoc.^mixin(X::Comp).new(payload => @args.join).throw }
    method block(*@) { __mutsu_slang_shim('block', self) }
    method signature(*@) { __mutsu_slang_shim('signature', self) }
    method trait(*@) { __mutsu_slang_shim('trait', self) }
    method routine-def($d?) { __mutsu_slang_shim('routine-def', self) }
    method method-def($d?) { __mutsu_slang_shim('method-def', self) }
    # `<name=.key-origin('rule', 'keyword')>`: the core definition rule `rule`
    # (`routine-def`, `method-def`), matched where the cursor is; `keyword` names the
    # declarator whose origin it records.
    method key-origin(Str $rule, Str $keyword) { self."$rule"($keyword) }
    method statementlist(*@) { __mutsu_slang_shim('statementlist', self) }
    method statement(*@) { __mutsu_slang_shim('statement', self) }
    # `<blorst>`: a block, or one statement (a `Statement::Expression`).
    method blorst(*@) { __mutsu_slang_shim('blorst', self) }
    # `<variable>`: `$x`, `@a`, `%h`, `&f`.
    method variable(*@) { __mutsu_slang_shim('variable', self) }
    method arglist(*@) { __mutsu_slang_shim('arglist', self) }
    # The `for` header's pieces: its keyword, the iterated expression (up to the
    # block) and the block, which may be a pointy one.
    token block-for { 'for' }
    # The checks for Perl 5 `for` spellings are the core parser's; here nothing to vet.
    token vetPerlForSyntax { <?> }
    method EXPR(*@) { __mutsu_slang_shim('EXPR', self) }
    method pointy-block(*@) { __mutsu_slang_shim('pointy-block', self) }
    # `<blockoid>`: the braces and statements of a block, as a `Blockoid`.
    method blockoid(*@) { __mutsu_slang_shim('blockoid', self) }
    # The name a declarator takes (`<.identifier>`).
    token identifier { <.ident> [ <[\-']> <.ident> ]* }
    # A package name as a declarator takes it (`A::B`, `GLOBAL::C`), with its
    # adverbs (`Foo:ver<1>`).
    token longname { <name> <colonpair>* }
    token name { <identifier> [ '::' <identifier> ]* }
    # A type name (`Int`, `Foo::Bar`), as a `Type::Simple`.
    token typename { <.identifier> [ '::' <.identifier> ]* }
    # `:name`, `:!name`, `:name(...)`, `:name<...>`.
    token colonpair {
        ':' $<neg>='!'? <identifier>
        [ '(' <-[)]>* ')' | '<' <-[>]>* '>' ]?
    }
    # The scope a declarator opens: `$*BLOCK` is the node it builds, a `kind`
    # (`Method`, `Sub`, ...) with the signature and body a later step fills in.
    token enter-block-scope($kind) {
        <?{ $*BLOCK = ::("RakuAST::$kind").new; True }>
    }
    token leave-block-scope { <?> }
    # `self.Nodify('ParseTime')`: the RakuAST class or role of that name.
    method Nodify(Str $name) { ::("RakuAST::$name") }
    # `<.kok>`: the end of a declarator keyword, then whitespace.
    token kok { <.end-keyword> <.ws> }
    # `<.set_braid_from(self)>` hands the braid of the current cursor to the
    # production's sub-parse; the host grammar has one braid, so it is a
    # zero-width success.
    token set_braid_from($) { <?> }
    # `<rules=.FOREIGN-LANG('Grammar::BNF', 'main_syntax')>`: the rule `$rule` of
    # the language the slang registered as `$lang` (ADR-17741 §2.2), parsed with
    # its own actions where the cursor stands. The Match it answers spans what
    # that grammar consumed, and its `.made` is what those actions made.
    method FOREIGN-LANG($lang, $rule, *%) {
        my ($grammar, $actions) = __mutsu_slang_named_language($lang);
        $grammar.subparse(self.orig, :pos(self.pos), :$rule, :$actions)
    }
    method FOREIGN_LANG($lang, $rule, *%) { self.FOREIGN-LANG($lang, $rule) }
}
