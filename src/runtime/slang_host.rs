//! The slang host (ADR-15762 §2.2): the interpreter that loaded a slang module
//! stays alive for the rest of the unit's parse and runs the slang's tokens and
//! actions when the parser reaches a hookable production.
//!
//! The parser thread and the host thread alternate on a strict call stack over
//! two channels, so neither touches the other's state:
//!
//! ```text
//! parser  --Hook{rule, source}------------------>  host   runs the token
//! parser  <--BackCall{rule, pos}----------------   host   a core rule (`<block>`)
//! parser  --BackReply(end, node)---------------->  host   needs the parser
//! parser  <--Done(end, node)--------------------   host   token + action done
//! ```
//!
//! Positions are character offsets into `source`, the text from the start of
//! the production to the end of the unit. The host owns the composed grammar
//! and actions objects; a RakuAST node crosses the boundary as the `Value` it
//! is (both threads are registered GC mutators).
//!
//! The channel types are in [`crate::slang_link`], a leaf the parser may name.

use super::*;
use std::cell::RefCell;
use crate::slang_link::{FromHost, HostAnswer, HostEnd, ToHost};

thread_local! {
    static HOST_LINK: RefCell<Option<HostEnd>> = const { RefCell::new(None) };
    /// The entry of the Raku side of the host, evaluated on the first hook.
    static HOST_ENTRY: RefCell<Option<Value>> = const { RefCell::new(None) };
    /// How many hooks are running inside one another (a slang production in a
    /// block another one's shim parsed).
    static HOOK_DEPTH: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
}

/// Whether a slang hook is running on this thread: the host sees none of the
/// importing unit's lexicals, so it must not judge a variable undeclared.
// Cost: O(1).
pub(super) fn in_hook() -> bool {
    HOOK_DEPTH.with(std::cell::Cell::get) > 0
}

/// The bound on nested hooks: the source's nesting is untrusted input
/// (docs/security.md, resource bounds), so a deeper one is a catchable error.
const MAX_HOOK_DEPTH: u32 = 64;

/// The Raku side of the host: the base grammar and actions the slang's roles
/// are composed onto, and the entry the loop calls (ADR-15762 §2.3, §2.4).
/// `block`, `signature` and `trait` are shims: the core production is the Rust parser's, reached by a
/// back-call. `end-keyword` is rakudo's own token.
const HOST_PRELUDE: &str = r#"
grammar Mutsu::Slang::Host::Base {
    token end-keyword { » <!before \h* '=>'> }
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
}
"#;

/// The stubs of the resolver, compilation unit and base actions a production reads
/// (`$*R`, `$*CU`, `attach`). The unit evaluates them too, to replay an action there
/// (ADR-17243).
pub(super) const HOST_STUBS: &str = r#"
# The resolver and compilation unit a production reads (`$*R`, `$*CU`): the host
# resolves names at run time, so declaring a lexical is a no-op and the
# context is empty.
class Mutsu::Slang::Host::Resolver {
    has @.generated;
    method declare-lexical($declaration) { $declaration }
    method create-scope-implicits() { }
    # `$*R.current-scope.merge-generated-lexical-declaration($decl)`: a constant
    # a production installs for the unit (Qwiratry's `aggsub` class). The host
    # keeps it; the run binds it in the unit beside the statement it returns.
    method current-scope() { self }
    method merge-generated-lexical-declaration($declaration, :$resolver) {
        @!generated.push($declaration);
    }
}
class Mutsu::Slang::Host::CompUnit { method context() { Nil } }
class Mutsu::Slang::Host::Actions {
    method typename(Mu $/) {
        $/.make(RakuAST::Type::Simple.new(RakuAST::Name.from-identifier(~$/)));
    }
    method attach($/, $ast) { $/.make($ast); $ast }
    # The core actions a slang may override and `callsame` into: the core node
    # is the Match's `.ast` already, so they answer it.
    method INFIX-EXPR(Mu $/) { $/.ast }
    method PREFIX-EXPR(Mu $/) { $/.ast }
    method POSTFIX-EXPR(Mu $/) { $/.ast }
    method numish(Mu $/) { $/.ast }
    # The block statements a slang's actions may override.
    method statement-control:sym<given>(Mu $/) { $/.ast }
    # A `for` a slang's own token matched (its header is the shims' nodes) has
    # no core node yet; one the core parser matched is the Match's `.ast`.
    method statement-control:sym<for>(Mu $/) {
        return $/.ast unless $<EXPR>.defined && $<pointy-block>.defined;
        my $node = RakuAST::Statement::For.new(
            source => $<EXPR>.ast, body => $<pointy-block>.ast);
        $/.make($node);
        $Mutsu::Slang::Host::Built = $node;
        $node
    }
    method statement-control:sym<if>(Mu $/) { $/.ast }
    method statement-control:sym<while>(Mu $/) { $/.ast }
}
"#;

/// The entry of the host: runs one production and answers its node.
const HOST_ENTRY_SOURCE: &str = r#"
our $Mutsu::Slang::Host::G;
our $Mutsu::Slang::Host::Built;
our $Mutsu::Slang::Host::A;
sub mutsu-slang-host-run(Str $rule, Str $source, @groles, @aroles, :$node) {
    unless $Mutsu::Slang::Host::G.defined {
        my $g = Metamodel::GrammarHOW.new_type(:name<Raku::Grammar>);
        $g.^add_parent(Mutsu::Slang::Host::Base);
        $g.^add_role($_) for @groles;
        $g.^compose;
        my $a = Metamodel::ClassHOW.new_type(:name<Raku::Actions>);
        $a.^add_parent(Mutsu::Slang::Host::Actions);
        $a.^add_role($_) for @aroles;
        $a.^compose;
        $Mutsu::Slang::Host::G = $g;
        $Mutsu::Slang::Host::A = $a.new;
    }
    if $node.defined {
        # A numeric literal's `$/` is the text it was written as, holding the
        # capture the core grammar's `numish` has (`integer`, `decimal-number`,
        # `radix-number`), which an override reads by key.
        my %hash;
        if $rule eq 'numish' && $source.chars {
            my $part = Match.new(:orig($source), :from(0), :to($source.chars));
            # Only an ASCII integer, decimal or radix literal has a capture; `NaN`,
            # `Inf`, `∞`, a unum (`Ⅼ`, `⅔`), `<1/2>` and `<1+2i>` are numish alone.
            my $key = $source.starts-with(':') ?? 'radix-number'
                !! $source ~~ /^ [ 0 <[xXoObB]> <[0..9a..fA..F_]>+ | <[0..9]> <[0..9_]>* ] $/ ?? 'integer'
                !! $source ~~ /^ <[0..9.]> <[0..9_.eE+\-]>* $/ ?? 'decimal-number'
                !! Nil;
            %hash{$key} = $part if $key;
        }
        my $core = Match.new(:orig($source), :from(0), :to($source.chars), :hash(%hash));
        $core.make($node);
        $Mutsu::Slang::Host::A."$rule"($core);
        return ($core.to, $core.ast);
    }
    my $*R = Mutsu::Slang::Host::Resolver.new;
    my $*CU = Mutsu::Slang::Host::CompUnit.new;
    $Mutsu::Slang::Host::Built = Nil;
    my $m = $Mutsu::Slang::Host::G.subparse($source, :rule($rule), :actions($Mutsu::Slang::Host::A));
    return Nil unless $m;
    # A node the core action built for a production the slang's own token matched
    # (a `for` with the slang's extra clause) stands for it: a slang action that
    # only `callsame`s into the core one, or that attached nothing, leaves it.
    # (A shim leaves the last core production's node on the cursor, so `.ast`
    # alone cannot tell.)
    if !$Mutsu::Slang::Host::Built.defined && !$m.ast.defined && Mutsu::Slang::Host::Actions.can($rule) {
        Mutsu::Slang::Host::Actions.new."$rule"($m);
    }
    if $Mutsu::Slang::Host::Built.defined {
        return ($m.to, $Mutsu::Slang::Host::Built);
    }
    # Constants the production installed become `my constant NAME = VALUE`
    # declarations of the unit, standing for the statement.
    if $*R.generated.elems && $m.ast.defined {
        # A type the production minted is rebuilt in the unit by replaying the
        # recorded metamodel calls (ADR-13370); other values travel as they are.
        my $log := __mutsu_slang_class_log();
        my @decls = $*R.generated.map: -> $d {
            my sub literal($value) {
                RakuAST::Term::Declaration.new(RakuAST::Declaration::ResolvedConstant.new(:compile-time-value($value)))
            }
            my $init := $log.elems && !$d.value.defined
                ?? RakuAST::Call::Name.new(
                    name => RakuAST::Name.from-identifier('__mutsu_slang_replay_class'),
                    args => RakuAST::ArgList.new(literal($log), literal($d.value)))
                !! literal($d.value);
            RakuAST::Statement::Expression.new(expression => RakuAST::VarDeclaration::Constant.new(
                name => $d.name,
                initializer => RakuAST::Initializer::Assign.new($init)))
        };
        # TODO: a production installing several constants needs a statement
        # group the hook can lower; none exists, so it is refused rather than
        # dropping all but one.
        die "slang production installed {@decls.elems} generated lexicals; only one is supported" if @decls.elems > 1;
        my $stmt := @decls.head;
        return ($m.to, RakuAST::StatementList.new($stmt));
    }
    # An action of the slang's own production may have done more than attach a node
    # (Qwiratry's `mold` registers into a module-level registry): that happened in
    # this interpreter only, so the unit re-runs the recorded actions where the
    # production stands (ADR-17243). A class the production installed as a constant has its own replay, above.
    if $m.ast.defined && $rule.split(':')[0] eq any(<routine-declarator scope-declarator statement-control statement-prefix>) {
        my @events = __mutsu_slang_action_events();
        if @events.elems {
            my sub literal($value) {
                RakuAST::Term::Declaration.new(RakuAST::Declaration::ResolvedConstant.new(:compile-time-value($value)))
            }
            my $replay = RakuAST::Statement::Expression.new(expression => RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('__mutsu_slang_replay_actions'),
                args => RakuAST::ArgList.new(literal(@aroles), literal(@events))));
            return ($m.to, RakuAST::StatementList.new($replay, $m.ast));
        }
    }
    return ($m.to, $m.ast) if $m.ast.defined;
    # A production whose slang attached no node stands for the core definition it
    # read (rakudo's own `routine-declarator` action answers it): `Slang::Mosdef`
    # has no actions role at all.
    for <routine-def method-def package-def> -> $key {
        my $part = $m.hash{$key};
        return ($m.to, $part.ast) if $part.defined && $part.ast.defined;
    }
    ($m.to, $m.ast)
}
&mutsu-slang-host-run
"#;

impl Interpreter {
    /// Serve parser requests until the parser drops its end (the unit's parse
    /// is over). The slang's roles were recorded by `define_slang`.
    // Cost: the slang's own code per request.
    pub(super) fn serve_slang_host(&mut self) {
        loop {
            let message = HOST_LINK.with(|l| {
                let link = l.borrow();
                let link = link.as_ref()?;
                crate::gc::block_quiescent(|| link.from_parser.recv()).ok()
            });
            let Some(ToHost::Hook { rule, source, node }) = message else {
                return;
            };
            if !self.answer_hook(&rule, &source, node) {
                return;
            }
        }
    }

    /// Run one hook and send the parser its `Done`; `false` when the parser is
    /// gone.
    // Cost: the token and the action.
    fn answer_hook(&mut self, rule: &str, source: &str, node: Option<Value>) -> bool {
        let depth = HOOK_DEPTH.with(std::cell::Cell::get);
        let answer = if depth >= MAX_HOOK_DEPTH {
            Err(format!(
                "slang productions nest deeper than {MAX_HOOK_DEPTH} levels"
            ))
        } else {
            HOOK_DEPTH.with(|d| d.set(depth + 1));
            super::slang_class_replay::begin_class_log();
            let answer = self.run_slang_hook(rule, source, node);
            super::slang_class_replay::end_class_log();
            HOOK_DEPTH.with(|d| d.set(depth));
            answer
        };
        HOST_LINK.with(|l| {
            l.borrow()
                .as_ref()
                .is_some_and(|link| link.to_parser.send(FromHost::Done(answer)).is_ok())
        })
    }

    /// Match `rule` at the start of `source` with the composed grammar and
    /// read the node its action attached.
    // Cost: the token and the action.
    fn run_slang_hook(&mut self, rule: &str, source: &str, node: Option<Value>) -> HostAnswer {
        let mut run = HOST_ENTRY.with(|e| e.borrow().clone());
        if run.is_none() {
            let entry = self
                .eval_code_string(&format!("{HOST_PRELUDE}{HOST_STUBS}{HOST_ENTRY_SOURCE}"))
                .map_err(|e| e.message.to_string())?;
            HOST_ENTRY.with(|e| *e.borrow_mut() = Some(entry.clone()));
            run = Some(entry);
        }
        let run = run.unwrap_or(Value::NIL);
        let grammar_roles = Value::array(self.regex_state.slang_grammar_roles.clone());
        let action_roles = Value::array(self.regex_state.slang_action_roles.clone());
        let answer = self
            .call_sub_value(
                run,
                {
                    let mut args = vec![
                        Value::str(rule.to_string()),
                        Value::str(source.to_string()),
                        grammar_roles,
                        action_roles,
                    ];
                    if let Some(node) = node {
                        args.push(Value::pair("node".to_string(), node));
                    }
                    args
                },
            )
            .map_err(|e| e.message.to_string())?;
        let Some(items) = answer.as_list_items() else {
            return Ok(None);
        };
        let end = items
            .first()
            .and_then(Value::as_int)
            .filter(|n| *n >= 0)
            .ok_or_else(|| "slang token answered no end position".to_string())?;
        let node = items.get(1).cloned().unwrap_or(Value::NIL);
        // A node a class wrapped (an instance over a `RakuAst` base) stands for
        // that base.
        let node = match node.view() {
            ValueView::Instance { attributes, .. } => attributes.rakuast_base().unwrap_or(node),
            _ => node,
        };
        if !matches!(node.view(), ValueView::RakuAst(_)) {
            return Err(format!(
                "slang production '{rule}' produced no RakuAST node (its action must attach one)"
            ));
        }
        Ok(Some((end as usize, node)))
    }

    /// `$grammar.HOW.find_method($grammar, 'statementlist')` on a slang grammar
    /// handle: the core production's method, which a slang's override captures
    /// and delegates to (ADR-16888 §2.3). A `Method` (EEL tests `~~ Method`)
    /// invoked with the cursor as its invocant, which asks the parser to parse
    /// the core rule at it; extra arguments (`arglist(|args)`) are ignored.
    // Cost: O(1) (compiles a one-line method).
    pub(super) fn slang_core_method(&mut self, invocant: &Value, name: &str) -> Option<Value> {
        if !crate::parser::slang_host_wrapped_rule(name) {
            return None;
        }
        let ValueView::Instance { class_name, .. } = invocant.view() else {
            return None;
        };
        if class_name.as_str() != super::slang_activation::GRAMMAR_HANDLE_CLASS {
            return None;
        }
        self.eval_code_string(&format!(
            "method (*@) {{ __mutsu_slang_shim('{name}', self) }}"
        ))
        .ok()
    }

    /// `__mutsu_slang_shim($rule, $cursor)`: the grammar's shim for a core
    /// production. Asks the parser (a back-call) to parse `$rule` at the
    /// cursor's position and answers a cursor spanning it whose `.ast` is the
    /// node the parser built.
    // Cost: the parser's own parse of the production.
    pub(super) fn builtin_slang_shim(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let rule = args.first().map(Value::to_string_value).unwrap_or_default();
        let cursor = args.get(1).cloned().unwrap_or(Value::NIL);
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = cursor.view()
        else {
            return Err(RuntimeError::new("slang shim called without a cursor"));
        };
        let pos = attributes
            .as_map()
            .get("pos")
            .and_then(Value::as_int)
            .unwrap_or(0)
            .max(0) as usize;
        let orig = attributes
            .as_map()
            .get("orig")
            .map(Value::to_string_value)
            .unwrap_or_default();
        let answer = self.slang_back_call(&rule, pos)?;
        let class = class_name.resolve();
        match answer.map_err(RuntimeError::new)? {
            Some((end, node)) => {
                let cursor = Self::make_cursor_value(&class, &orig, pos as i64, end as i64, false);
                if let ValueView::Instance { attributes, .. } = cursor.view() {
                    attributes.insert("ast", node);
                }
                Ok(cursor)
            }
            None => Ok(Self::make_cursor_value(
                &class, &orig, pos as i64, -1, true,
            )),
        }
    }

    /// Ask the parser to parse the core production `rule` at `pos` and wait for
    /// its answer, serving a hook the parser starts meanwhile: a slang
    /// production inside the block it parses runs inside this call.
    // Cost: the parser's own parse of the production.
    fn slang_back_call(&mut self, rule: &str, pos: usize) -> Result<HostAnswer, RuntimeError> {
        let lost = || RuntimeError::new("the parser stopped answering the slang shim");
        HOST_LINK.with(|l| {
            let link = l.borrow();
            let link = link.as_ref().ok_or_else(lost)?;
            link.to_parser
                .send(FromHost::BackCall {
                    rule: rule.to_string(),
                    pos,
                })
                .map_err(|_| lost())
        })?;
        loop {
            let message = HOST_LINK.with(|l| {
                let link = l.borrow();
                let link = link.as_ref()?;
                crate::gc::block_quiescent(|| link.from_parser.recv()).ok()
            });
            match message.ok_or_else(lost)? {
                ToHost::BackReply(answer) => return Ok(answer),
                ToHost::Hook { rule, source, node } => {
                    if !self.answer_hook(&rule, &source, node) {
                        return Err(lost());
                    }
                }
            }
        }
    }
}

/// Keep the host's end of the connection on this (the host) thread.
pub(super) fn install_host_end(end: HostEnd) {
    HOST_LINK.with(|l| *l.borrow_mut() = Some(end));
}
