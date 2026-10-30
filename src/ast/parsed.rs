//! Construct families the parser builds in their RakuAST shape
//! (ADR-10723 Stage 2, §4 "Typed nodes vs. the uniform `RakuAstNode`").
//!
//! A migrated family's parse function builds a typed node shaped like the
//! RakuAST node (`Statement::If` keeps its condition, its `then` block, its
//! `elsif`/`orwith` clauses and its `else` block) and records the source
//! positions Rakudo gives those nodes. Children of a family that has not
//! migrated yet stay ordinary `Expr`/`Stmt` values.
//!
//! An executing parse hands the node to `rakuast::lower` where it is built, so
//! the compiler and every analysis see the tree they always saw. A parse whose
//! tree only the RakuAST conversion reads (`ast::spelled::keeping`) keeps the
//! node in [`Stmt::Ast`]; `.AST` renders it, positions included, and the
//! `MUTSU_RAKUAST` round trip lowers it through the same function.

use super::{Expr, ParamDef, SrcSpan, Stmt};

/// A statement-level construct kept in its RakuAST shape.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ParsedStmt {
    /// The traits written on a routine or package, in source order; opens
    /// its body. Compiler-inert: the execution fields the parser folds them
    /// into are the code, this is what the source said. Analyses skip it
    /// unless they opt in (`Visit::visit_routine_traits`); lowering it
    /// produces no statement.
    RoutineTraits(Vec<super::routine_trait::RoutineTrait>),
    /// `if` / `unless` / `with` / `without` and their clause chain.
    Conditional(Conditional),
    /// `given TOPIC BLOCK` (`Statement::Given`); the block's topic parameter,
    /// if any, is `BlockParams::Topic`.
    Given { topic: Expr, block: ParsedBlock },
    /// `when MATCHER BLOCK` (`Statement::When`).
    When { condition: Expr, block: ParsedBlock },
    /// `default BLOCK` (`Statement::Default`).
    Default(ParsedBlock),
    /// `while COND BLOCK` / `until COND BLOCK` (`Statement::Loop::While` /
    /// `::Until`).
    WhileLoop(WhileLoop),
    /// A bare `{ ... }` statement (`Statement::Expression` over a `Block`).
    BareBlock(ParsedBlock),
    /// `loop`, C-style or bare, and `repeat ... while` / `until`
    /// (`Statement::Loop`, `::Loop::RepeatWhile` / `::RepeatUntil`).
    Loop(LoopStmt),
    /// `for LIST BLOCK` (`Statement::For`). `Stmt::For` already keeps what
    /// the source wrote, so the node is that statement plus the positions of
    /// its block.
    For { stmt: Box<Stmt>, block: BlockSpans },
    /// A binding declaration, `my $x := EXPR` / `my @a := EXPR` / `my %h :=
    /// EXPR` (`VarDeclaration::Simple` with `Initializer::Bind`): the
    /// `Stmt::VarDecl` as written. `rakuast::lower` wraps it in the
    /// bookkeeping statements the compiler needs (`ast::bind_decl::expand`).
    BindDecl(Box<Stmt>),
    /// `my %h does Role`, `my @a does Role = 1, 2`: the declaration (a
    /// `Stmt::VarDecl`), the role as written and the `=` initializer.
    /// `rakuast::lower` mixes the role in between the two
    /// (`ast::var_does::expand`).
    DoesDecl {
        decl: Box<Stmt>,
        role: Expr,
        init: Option<Expr>,
    },
    /// `my $x will begin { ... }`: the declaration (a `Stmt::VarDecl`) and the
    /// `will PHASE BLOCK` traits as written (kind and body). `rakuast::lower`
    /// expands each into a phaser after the declaration
    /// (`rakuast::will_decl`).
    WillDecl {
        decl: Box<Stmt>,
        /// The declared variable the phasers' `$_` is bound to.
        name: String,
        phasers: Vec<(super::PhaserKind, Vec<Stmt>)>,
    },
    /// A declaration or assignment with a loose tail, `my $x = 1 and 2`: the
    /// head (see `ast::decl_tail::is_head`) and the tail, whose leftmost
    /// operand re-reads the variable the head declares. RakuAST makes the
    /// whole one expression with the head as that operand.
    DeclTail { head: Box<Stmt>, tail: Expr },
    /// A declaration whose initializer is a compound assignment, `my $x *= 5`
    /// / `has $!g //= 5`: the declaration (a `Stmt::VarDecl` or a private
    /// `Stmt::HasDecl`), the base infix (`*`, or a word or user infix such as
    /// `foo`) and the right-hand side. RakuAST makes the declaration the
    /// assignment's left operand; `rakuast::lower` expands it into the
    /// declaration followed by the compound assignment to it
    /// (`ast::decl_compound_assign::expand`).
    DeclCompound {
        decl: Box<Stmt>,
        op: String,
        source: Expr,
    },
    /// A sigilless declaration, `my \x = 5` / `my Int \x := $s` /
    /// `my \x .= new`, as written. `rakuast::lower` adds the bookkeeping the
    /// compiler needs (`rakuast::sigilless_lower::build_sigilless_bind_stmt`).
    SigillessDecl(Box<SigillessSource>),
    /// `temp my $x`: the declaration (a `Stmt::VarDecl`) under a `temp` prefix.
    /// `rakuast::lower` follows it with the save of the fresh variable
    /// (`ast::temporize::save`).
    TempDecl(Box<Stmt>),
    /// A package-like declaration (`class`, `grammar`, `module`, `package`,
    /// `role`) with `:ver<..>` / `:auth<..>` adverbs on its name or an
    /// `is export`: the declaration as written and its header. `rakuast::lower`
    /// adds the meta setters and the export registration
    /// (`ast::package_header::wrap`).
    PackageHeader {
        decl: Box<Stmt>,
        name: String,
        header: super::package_header::Header,
    },
    /// A statement with statement modifiers (`Statement::Expression`'s
    /// `condition-modifier` and `loop-modifier`).
    Modified(Modified),
    /// A named routine declared and called on the spot, `sub f($n) { ... }(21)`:
    /// the declaration (a `Stmt::SubDecl`) and the call arguments as written.
    /// `rakuast::lower` expands it into the declaration and a call of its name
    /// (`ast::immediate_call::expand`).
    ImmediateCall { decl: Box<Stmt>, args: Vec<Expr> },
    /// `take VALUE and|or|xor|andthen|orelse|notandthen TAIL`: the value, the
    /// loose operator and the tail as written. RakuAST makes the whole one
    /// infix application with the `take` call as its left operand;
    /// `rakuast::lower` evaluates the value once into a temporary, takes it,
    /// and runs the short-circuit tail (`ast::expand_take_logical_tail`).
    TakeLogicalTail {
        value: Expr,
        op: crate::token_kind::TokenKind,
        right: Expr,
    },
    /// `use Foo ..., proto sub MAIN(|) {*}`: the proto declaration that is part
    /// of the `use` argument, and the `use` statement. RakuAST has the proto as
    /// the argument itself; `rakuast::lower` declares it ahead of the import.
    InlineProtoUse {
        proto: Box<Stmt>,
        use_stmt: Box<Stmt>,
    },
    /// `done VALUE` in a supply or react block: the value as written.
    /// RakuAST keeps `emit VALUE; done` as two statements; `rakuast::lower`
    /// expands it to them (`ast::done_value::expand`).
    DoneValue(Expr),
    /// `my Int $c .= new(), 5, 6`: the `.=` declaration and each further list
    /// item as written. RakuAST has one comma application whose first operand
    /// is the declaration; `rakuast::lower` expands it into the declaration
    /// followed by the items as sunk statements.
    CallAssignList { head: Box<Stmt>, items: Vec<Expr> },
    /// A comma list of declarations, `my $x = 1, my $y = 2`, as written: one
    /// source statement whose execution is the declarations' expansions in
    /// order, with no new lexical scope (RakuAST's `ApplyListInfix` over the
    /// declarations).
    CommaDecls(Vec<Stmt>),
    /// `temp` / `let` over a variable or an element, with the assignment that
    /// follows, if any: RakuAST's prefix operator over the lvalue.
    /// `rakuast::lower` saves the variable and assigns inside the save
    /// (`ast::temporize::expand`).
    Temporize(Box<super::temporize::Temporized>),
    /// `has Type ($.a, $.b)`: one visible RakuAST signature declaration over
    /// the attribute declarations as written. `rakuast::lower` installs each
    /// listed attribute separately (a block of the declarations).
    AttributeGroup(Vec<Stmt>),
    /// A declaration parsed inside a public attribute's colon argument list
    /// (`has Int $.gid: has Str @.m;`), with the declaration that introduced
    /// the list last. The nested declarations execute, but RakuAST displays the
    /// declaration that introduced the colon list. Lowers to a block of them.
    AttributeColonArglist(Vec<Stmt>),
    /// `my ($a, @b) = RHS` (also `:=`, `state`, `our`, a typed or nested
    /// group): the declaration as written. `rakuast::lower` expands it into
    /// ordinary declarations reading a staging temporary
    /// (`rakuast::signature_expand::signature_decl`). The parser holds it unlowered
    /// (see `rakuast::signature_expand::pending`) until the
    /// statement modifiers and loose tails that read it have been parsed.
    SignatureDecl(Box<super::SignatureDecl>),
    /// `:(...) := RHS`: the signature literal and the right-hand side as
    /// written. `rakuast::lower` expands the bind into temporary declarations
    /// and assignments (`ast::signature_bind::expand`).
    SignatureBind { signature: Expr, rhs: Expr },
    /// `my [Type] $x .= method(...)`: the declaration as written.
    /// `rakuast::lower` expands it (`ast::method_assign_decl::expand`).
    MethodAssignDecl(Box<super::method_assign_decl::MethodAssignDecl>),
}

/// `STMT if COND`, `STMT for LIST`, `STMT if COND for LIST`: the statement as
/// written and its modifiers. A statement takes at most one condition
/// modifier and one loop modifier, the condition applying first.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct Modified {
    pub(crate) stmt: Box<Stmt>,
    pub(crate) condition: Option<StatementModifier>,
    pub(crate) loop_: Option<StatementModifier>,
    /// The modified statement, from its first character to its last; the
    /// start is filled in by the statement parser, which knows it.
    pub(crate) body: SrcSpan,
}

/// One statement modifier: its keyword, its operand as written (no `!` for
/// `unless` / `until`) and where it was written, from the keyword to the end
/// of the operand.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct StatementModifier {
    pub(crate) kind: ModifierKind,
    pub(crate) expr: Expr,
    pub(crate) span: SrcSpan,
}

/// Which keyword a statement modifier was written with.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ModifierKind {
    If,
    Unless,
    When,
    With,
    Without,
    For,
    While,
    Until,
    Given,
}

impl ModifierKind {
    /// The modifier written as `keyword`, if it is one.
    // Cost: O(1).
    pub(crate) fn from_keyword(keyword: &str) -> Option<Self> {
        Some(match keyword {
            "if" => Self::If,
            "unless" => Self::Unless,
            "when" => Self::When,
            "with" => Self::With,
            "without" => Self::Without,
            "for" => Self::For,
            "while" => Self::While,
            "until" => Self::Until,
            "given" => Self::Given,
            _ => return None,
        })
    }

    /// Whether this is a loop modifier (`for` / `while` / `until` / `given`)
    /// rather than a condition modifier.
    // Cost: O(1).
    pub(crate) fn is_loop(self) -> bool {
        matches!(self, Self::For | Self::While | Self::Until | Self::Given)
    }
}

/// A sigilless declaration as written (see [`ParsedStmt::SigillessDecl`]).
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct SigillessSource {
    pub(crate) name: String,
    /// The initializer: the right-hand side, or for `.=` the method call on
    /// the declared type (`Mu` when untyped).
    pub(crate) expr: Expr,
    pub(crate) type_constraint: Option<String>,
    pub(crate) is_state: bool,
    pub(crate) is_our: bool,
    pub(crate) form: SigillessForm,
}

/// How a sigilless declaration was initialized.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum SigillessForm {
    /// `my \x = EXPR`.
    Assign,
    /// `my \x := EXPR`.
    Bind,
    /// `my \x .= method`.
    CallAssign,
}

/// An expression-level construct kept in its RakuAST shape.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ParsedExpr {
    /// `TARGET = SOURCE` (`ApplyInfix` over `Assignment`).
    Assign(Assign),
    /// `TARGET OP= SOURCE` (`ApplyInfix` over `MetaInfix::Assign`).
    CompoundAssign(CompoundAssign),
    /// `supply { BODY }`: the body as written, before the expansion rewrites
    /// its `emit` / `done` onto the on-demand emitter
    /// (`StatementPrefix::Supply`). It lowers to that expansion
    /// (`rakuast::supply_lower::supply_method_call`).
    Supply(Vec<Stmt>),
    /// The body of a `start { BODY }` whose placeholder Raku rejects, as
    /// written. It lowers to a do-block, so the compiler reports the
    /// diagnostic instead of `make_anon_sub` consuming the placeholder as a
    /// parameter.
    StartBody(Vec<Stmt>),
    /// `(LVALUES) »=» VALUE` over a literal list of lvalues (`ApplyInfix`
    /// over `Assignment` under a hyper meta-infix). It lowers to the
    /// element-wise expansion (`rakuast::hyper_assign::lower_hyper_assignment`).
    HyperAssign(HyperAssign),
    /// An inline `subset` used as a term, `subset :: of Int where * > 0`: the
    /// declaration as written. It lowers to the declaration followed by a
    /// read of its generated name (`ast::inline_subset_term::lower`).
    InlineSubset(Box<Stmt>),
    /// `-« @a`, `++<< @a`: a hyper prefix operator and its operand as written
    /// (`ApplyPrefix` over `MetaPrefix::Hyper`). It lowers to the runtime's
    /// element-wise prefix call (`rakuast::lower_hyper_prefix`).
    HyperPrefix(HyperPrefix),
    /// `⚛$x` and `$x ⚛= VALUE`: the atomic fetch of, and plain store into, a
    /// variable named by its storage name. They lower to the reserved calls
    /// the runtime executes (`ast::atomic_op::lower_var`).
    AtomicVar(AtomicVar),
    /// A subscript with the adverbs written after it, `@a[0]:exists`,
    /// `%h<a>:delete:k`, `@a[1]:foo` (`Postcircumfix::*Index` with its
    /// `colonpairs`). It lowers through the postcircumfix lowering
    /// (`rakuast::subscript_adverb::lower`).
    SubscriptAdverbs(SubscriptAdverbs),
    /// `:{ KEY => VALUE, ... }`: the entries of an object hash as written
    /// (`Circumfix::HashComposer(:object-hash)`). It lowers to the runtime's
    /// `Mu`-keyed hash construction (`rakuast::hash_literal::lower_composer`).
    ObjectHash(Vec<Expr>),
    /// `qw/a b/`, `qqww/a $b/`, `«a b»`, `<<a b>>`, `%h<<$k>>`: the words of a
    /// word quote and the quote as written (`QuotedString` with `words` /
    /// `quotewords` processors). It lowers to the runtime's word list
    /// (`rakuast::word_list::lower_word_list`).
    WordList(WordList),
    /// `qqw/a $x b/`: an interpolating `words` quote whose string is split
    /// into words at run time (`QuotedString` with `words` processors over its
    /// interpolation). It lowers to the runtime's words call
    /// (`rakuast::word_list::lower_words_atom`).
    WordsAtom(WordsAtom),
}

/// The words of a word quote and its source.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct WordList {
    /// The words, in source order.
    pub(crate) items: Vec<WordItem>,
    /// The quote has the `val` processor: its words are allomorphs, and a
    /// quote of one word is that word, not a list.
    pub(crate) val: bool,
    /// The quote as written.
    pub(crate) source: WordSource,
}

/// One word of a [`WordList`].
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum WordItem {
    /// A word that is its own value: a literal or a quoted word.
    Expr(Expr),
    /// An unquoted word of a `quotewords` quote that interpolates: the
    /// string it spells, whose literal runs glue to the values around them.
    Atom { string: Expr, allomorphic: bool },
}

/// The text of a word quote, for the RakuAST view.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum WordSource {
    /// Nothing interpolates or nests: one literal segment.
    Plain {
        /// `quotewords` (honours inner quotes) rather than `words`.
        quotewords: bool,
        text: Box<str>,
    },
    /// `«a $b "c d"»`, `qqww/a $b/`, `Qww/"c d"/`: the segments are derived
    /// from the text.
    Interpolating {
        text: Box<str>,
        /// The source delimiter for nested `<< >>` or `« »` word quotes.
        delimiter: Option<char>,
        /// Whether unquoted text interpolates (`qqww`/angle quotes, unlike `Qww`).
        interpolates: bool,
    },
}

/// An interpolating `words` string that is split into words when it runs.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct WordsAtom {
    pub(crate) string: Expr,
    /// The words are allomorphs (`val`).
    pub(crate) allomorphic: bool,
}

/// A subscript and the colonpairs written after it, in source order.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct SubscriptAdverbs {
    /// The subscript as written (`Index`, `MultiDimIndex`, or the `ZenSlice`
    /// of `@a[]:exists`).
    pub(crate) subscript: Expr,
    /// Each adverb as the colonpair expression the source spells
    /// (`Binary` over `=>`, with its [`super::BinaryForm`]).
    pub(crate) pairs: Vec<Expr>,
}

impl SubscriptAdverbs {
    /// Whether one of the adverbs is `:exists` (in any spelling).
    // Cost: O(a), a = adverbs.
    pub(crate) fn has_exists(&self) -> bool {
        self.pairs
            .iter()
            .any(|pair| super::subscript_adverb::pair_key(pair).as_deref() == Some("exists"))
    }
}

/// An atomic access of a variable by name.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum AtomicVar {
    /// `⚛$x`.
    Fetch { name: String },
    /// `$x ⚛= VALUE`.
    Store { name: String, value: Expr },
}

/// `OP« OPERAND`: the prefix symbol (`"-"`, `"++"`) and the operand.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct HyperPrefix {
    pub(crate) symbol: String,
    pub(crate) operand: Expr,
}

/// `(LVALUES) »=» VALUE`: the target, the value and the dwim flags as written.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct HyperAssign {
    pub(crate) target: Expr,
    pub(crate) value: Expr,
    pub(crate) dwim_left: bool,
    pub(crate) dwim_right: bool,
}

/// A compound assignment as written: the target, the infix it applies, how
/// the infix was spelled, and the source.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct CompoundAssign {
    pub(crate) target: Expr,
    /// The base infix, as `CompoundAssignOp::from_op_name` reads it (`+`,
    /// `//`, `min`), or the name of an infix with no built-in compound
    /// operator (`mod`, a user-declared `+%+`), which the assignment calls.
    pub(crate) op: String,
    pub(crate) meta: CompoundMeta,
    pub(crate) source: Expr,
}

/// How a compound assignment's infix was written. Every spelling assigns the
/// same way to a scalar; the tree keeps what the source wrote.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum CompoundMeta {
    /// `$x += 1`.
    Plain,
    /// `$x [+]= 1`, `$x [[+]]= 1`: the infix inside this many brackets.
    Bracketed(u8),
    /// `$x Z+= 1` on a scalar.
    Zip,
    /// `$s (|)= 5`, `$s ∪= 5`: a set operator, as `op` spells it.
    Set,
    /// `$x .= meth(args)`: `source` is the method call applied to the target
    /// as written, and `op` is `.`.
    Dotty,
    /// `.= meth(args)` written with no target: the topic's `.=` method call
    /// (`Term::TopicCall` of a `.=` dispatch). `target` is `$_`, `source` the
    /// call over it, and `op` is `.`.
    TopicDotty,
    /// `$x R-= $y`: the reversed infix, which assigns to its right operand
    /// (`$y -= $x`). Its `op` may be `=` (`$x R= $y`).
    Reverse,
    /// `@a [X+]= @b`: a metaoperator over `op` inside this many brackets.
    BracketedMeta(BracketedMeta, u8),
}

/// The metaoperator of a bracketed compound assignment (`[X+]=`, `[Z~]=`,
/// `[R-]=`), or none (`Reduce`: a bare infix in brackets that is not a
/// compound-assignment operator).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum BracketedMeta {
    Cross,
    Zip,
    Reverse,
    Reduce,
}

impl BracketedMeta {
    /// The metaoperator a spelling (`X`, `Z`, `R`, `reduce`) names.
    // Cost: O(1).
    pub(crate) fn from_spelling(spelling: &str) -> Option<Self> {
        match spelling {
            "X" => Some(Self::Cross),
            "Z" => Some(Self::Zip),
            "R" => Some(Self::Reverse),
            "reduce" => Some(Self::Reduce),
            _ => None,
        }
    }

    /// The metaoperator's spelling, as `Expr::MetaOp` keeps it.
    // Cost: O(1).
    pub(crate) fn spelling(self) -> &'static str {
        match self {
            Self::Cross => "X",
            Self::Zip => "Z",
            Self::Reverse => "R",
            Self::Reduce => "reduce",
        }
    }
}

/// An assignment as written: the target with its parentheses, and the source.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct Assign {
    pub(crate) target: Expr,
    pub(crate) source: Expr,
    /// Whether it is an item assignment (`Assignment.new(:item)`), decided
    /// from the target while the parser builds the node
    /// ([`super::left_sigil`]).
    pub(crate) item: bool,
}

impl Assign {
    /// [`Assign::written_at`], for a target that starts its own expression.
    // Cost: O(n), n = size of the target.
    pub(crate) fn written(target: Expr, source: Expr) -> Self {
        Self::written_at(None, target, source)
    }

    /// The assignment `target = source` as the parser read it, the target
    /// starting at `at` when the expression before it may have left a sigil.
    // Cost: O(n), n = size of the target.
    pub(crate) fn written_at(at: Option<&str>, target: Expr, source: Expr) -> Self {
        let item = super::left_sigil::assignment_is_item(at, &target);
        Self {
            target,
            source,
            item,
        }
    }
}

/// Where a block was written: the block (from `->` when pointy), its braces and
/// its statements. See [`ParsedBlock`].
#[derive(Debug, Clone, Copy, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct BlockSpans {
    pub(crate) span: SrcSpan,
    pub(crate) blockoid: SrcSpan,
    pub(crate) list: SrcSpan,
}

/// Where a routine declaration (`Sub`, `Method`, `Submethod`) was written,
/// below the statement it is: its name, its signature and its block. Kept on
/// `Stmt::SubDecl` / `Stmt::MethodDecl`, which already hold what the source
/// wrote, so the declaration is its own typed node (ADR-10723 §4).
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct RoutineSpans {
    /// The declared name (`foo`, `p` of `!p`).
    pub(crate) name: SrcSpan,
    /// From the first parameter to the end of the last; an empty `( )` sits at
    /// its `)`, and a routine with no parentheses has none.
    pub(crate) signature: SrcSpan,
    pub(crate) block: BlockSpans,
    /// The traits written after the signature, in source order, one per
    /// trait the `.AST` parse records (`ast::routine_trait`).
    #[serde(default)]
    pub(crate) traits: Vec<TraitSpans>,
}

/// Where a trait was written (`is copy`, `returns Int`): all of it, and the
/// name or type it names.
#[derive(Debug, Clone, Copy, Default, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct TraitSpans {
    pub(crate) whole: SrcSpan,
    pub(crate) name: SrcSpan,
}

/// Where a package declaration (`class`, `grammar`, `role`) was written
/// below its statement: its name and its block.
#[derive(Debug, Clone, Copy, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct PackageSpans {
    pub(crate) name: SrcSpan,
    pub(crate) block: BlockSpans,
}

/// Where a type declaration that is not a package (`enum`, `subset`) was
/// written below its statement: its name, and a subset's `of` trait.
#[derive(Debug, Clone, Copy, Default, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct TypeDeclSpans {
    pub(crate) name: SrcSpan,
    /// `of TYPE` (a subset's base type as written); none when not written.
    pub(crate) of: TraitSpans,
    /// `is export` on a subset; none when not written.
    #[serde(default)]
    pub(crate) export: TraitSpans,
}

/// Where a parameter was written: all of it (type, traits and default
/// included) and its target variable.
///
/// Like `SrcSpan`'s `Hash`, its `Debug` leaves the positions out: the runtime
/// still tells two signatures apart by their `Debug` text in places (a forward
/// declaration and its definition, a role's re-registered method), and a
/// signature is the same wherever it was written.
#[derive(Clone, Default, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct ParamSpans {
    pub(crate) param: SrcSpan,
    pub(crate) target: SrcSpan,
    /// The nominal type, smiley included (`Int:D`).
    pub(crate) type_: SrcSpan,
    /// The `is` traits, in the order of `ParamDef::traits`.
    #[serde(default)]
    pub(crate) traits: Vec<TraitSpans>,
}

impl std::fmt::Debug for ParamSpans {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ParamSpans")
    }
}

/// Which loop a [`LoopStmt`] is.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum LoopKind {
    /// `loop { }` / `loop (init; cond; step) { }`.
    Loop,
    RepeatWhile,
    RepeatUntil,
}

/// `loop` / `repeat`. A `repeat` keeps its condition as written (no `!` for
/// `until`) and its pointy parameter as the block's `BlockParams::Topic`.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct LoopStmt {
    pub(crate) kind: LoopKind,
    pub(crate) init: Option<Box<Stmt>>,
    pub(crate) condition: Option<Expr>,
    pub(crate) step: Option<Expr>,
    pub(crate) block: ParsedBlock,
    pub(crate) label: Option<String>,
    /// Written as a term without `do` (`(while COND { ... })`); see
    /// `Stmt::While::is_bare_term`.
    pub(crate) bare_term: bool,
}

/// `while` / `until` with its optional label.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct WhileLoop {
    pub(crate) until: bool,
    /// The condition as written: no `!` for `until`.
    pub(crate) condition: Expr,
    pub(crate) block: ParsedBlock,
    pub(crate) label: Option<String>,
    /// Written as a term without `do` (`(while COND { ... })`); see
    /// `Stmt::While::is_bare_term`.
    pub(crate) bare_term: bool,
}

/// Which keyword opened a conditional statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ConditionalKind {
    If,
    Unless,
    With,
    Without,
}

/// Which keyword opened a continuation clause.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum ClauseKind {
    Elsif,
    Orwith,
}

/// `Statement::If` / `::Unless` / `::With` / `::Without`.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct Conditional {
    pub(crate) kind: ConditionalKind,
    /// The condition as written: no `!` for `unless`, no `.defined` test for
    /// `with`.
    pub(crate) condition: Expr,
    /// The `then` block (`body` for `unless` / `without`).
    pub(crate) then: ParsedBlock,
    /// The `elsif` / `orwith` clauses, in source order.
    pub(crate) clauses: Vec<ConditionalClause>,
    /// The `else` block.
    pub(crate) else_block: Option<ParsedBlock>,
}

/// `Statement::Elsif` / `Statement::Orwith`.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct ConditionalClause {
    pub(crate) kind: ClauseKind,
    pub(crate) condition: Expr,
    pub(crate) then: ParsedBlock,
}

/// The parameters written in front of a block, as the parser read them.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) enum BlockParams {
    /// A plain `{ ... }`.
    None,
    /// `-> PARAMS { ... }` after `if` / `elsif` / `else` / `unless`: the
    /// parameter list (empty for `-> { }`).
    List(Vec<ParamDef>),
    /// `-> PARAM { ... }` after `with` / `orwith` / `without`, or after the
    /// `else` of a `with` with no clauses: the single topic parameter (both
    /// `None` for `-> { }`).
    Topic {
        name: Option<String>,
        def: Option<Box<ParamDef>>,
    },
}

/// A `Block` / `PointyBlock` with its `Blockoid` and `StatementList`.
#[derive(Debug, Clone, Hash, serde::Serialize, serde::Deserialize)]
pub(crate) struct ParsedBlock {
    pub(crate) params: BlockParams,
    pub(crate) body: Vec<Stmt>,
    /// The whole block: from `{`, or from `->` for a pointy block, through `}`.
    pub(crate) span: SrcSpan,
    /// The braces, `{` through `}` (the `Blockoid`).
    pub(crate) blockoid: SrcSpan,
    /// The statements: from the first statement to the end of the last one,
    /// its `;` included; empty just after `{` when there is none.
    pub(crate) list: SrcSpan,
}

impl ParsedStmt {
    /// The statement a node of a family whose internal form is already its
    /// source form wraps (`for`): passes treat the node as that statement.
    // Cost: O(1).
    pub(crate) fn inner_stmt(&self) -> Option<&Stmt> {
        match self {
            ParsedStmt::For { stmt, .. } | ParsedStmt::BindDecl(stmt) => Some(stmt),
            ParsedStmt::PackageHeader { decl, .. }
            | ParsedStmt::TempDecl(decl)
            | ParsedStmt::ImmediateCall { decl, .. } => Some(decl),
            _ => None,
        }
    }

    /// Whether this is a `while` / `until` / `loop` / `repeat` written as a
    /// term without a `do`.
    // Cost: O(1).
    pub(crate) fn is_bare_loop_term(&self) -> bool {
        match self {
            ParsedStmt::WhileLoop(w) => w.bare_term,
            ParsedStmt::Loop(l) => l.bare_term,
            _ => false,
        }
    }

    /// Record that this loop is written as a term without a `do`.
    // Cost: O(1).
    pub(crate) fn mark_bare_loop_term(&mut self) {
        match self {
            ParsedStmt::WhileLoop(w) => w.bare_term = true,
            ParsedStmt::Loop(l) => l.bare_term = true,
            _ => {}
        }
    }

    /// [`ParsedStmt::inner_stmt`], mutably.
    // Cost: O(1).
    pub(crate) fn inner_stmt_mut(&mut self) -> Option<&mut Stmt> {
        match self {
            ParsedStmt::For { stmt, .. } | ParsedStmt::BindDecl(stmt) => Some(stmt),
            ParsedStmt::PackageHeader { decl, .. }
            | ParsedStmt::TempDecl(decl)
            | ParsedStmt::ImmediateCall { decl, .. } => Some(decl),
            _ => None,
        }
    }

    /// The expressions that run in the enclosing scope: every condition, topic
    /// and matcher (none for a node with an [`inner_stmt`](Self::inner_stmt)).
    // Cost: O(c), c = clauses.
    pub(crate) fn headers(&self) -> Vec<&Expr> {
        match self {
            ParsedStmt::Conditional(c) => std::iter::once(&c.condition)
                .chain(c.clauses.iter().map(|clause| &clause.condition))
                .collect(),
            ParsedStmt::Given { topic, .. } => vec![topic],
            ParsedStmt::When { condition, .. } => vec![condition],
            ParsedStmt::WhileLoop(w) => vec![&w.condition],
            ParsedStmt::Loop(l) => l.condition.iter().chain(l.step.iter()).collect(),
            ParsedStmt::For { .. }
            | ParsedStmt::BindDecl(_)
            | ParsedStmt::TempDecl(_)
            | ParsedStmt::AttributeGroup(_)
            | ParsedStmt::AttributeColonArglist(_)
            | ParsedStmt::MethodAssignDecl(_)
            | ParsedStmt::SignatureBind { .. }
            | ParsedStmt::SignatureDecl(_)
            | ParsedStmt::Temporize(_)
            | ParsedStmt::RoutineTraits(_)
            | ParsedStmt::CommaDecls(_) => Vec::new(),
            ParsedStmt::TakeLogicalTail { value, right, .. } => vec![value, right],
            ParsedStmt::DoneValue(value) => vec![value],
            ParsedStmt::InlineProtoUse { .. } => Vec::new(),
            ParsedStmt::CallAssignList { items, .. } => items.iter().collect(),
            ParsedStmt::ImmediateCall { args, .. } => args.iter().collect(),
            ParsedStmt::DoesDecl { role, init, .. } => std::iter::once(role).chain(init).collect(),
            ParsedStmt::WillDecl { .. } => Vec::new(),
            ParsedStmt::DeclTail { tail, .. } => vec![tail],
            ParsedStmt::DeclCompound { source, .. } => vec![source],
            ParsedStmt::SigillessDecl(d) => vec![&d.expr],
            ParsedStmt::PackageHeader { header, .. } => {
                header.adverbs.iter().map(|(_, value)| value).collect()
            }
            ParsedStmt::Modified(m) => m.modifiers().map(|modifier| &modifier.expr).collect(),
            ParsedStmt::Default(_) | ParsedStmt::BareBlock(_) => Vec::new(),
        }
    }

    /// Every block, in source order.
    // Cost: O(c), c = clauses.
    pub(crate) fn blocks(&self) -> Vec<&ParsedBlock> {
        match self {
            ParsedStmt::Conditional(c) => std::iter::once(&c.then)
                .chain(c.clauses.iter().map(|clause| &clause.then))
                .chain(c.else_block.iter())
                .collect(),
            ParsedStmt::Given { block, .. }
            | ParsedStmt::When { block, .. }
            | ParsedStmt::Default(block)
            | ParsedStmt::BareBlock(block) => vec![block],
            ParsedStmt::WhileLoop(w) => vec![&w.block],
            ParsedStmt::Loop(l) => vec![&l.block],
            ParsedStmt::For { .. }
            | ParsedStmt::BindDecl(_)
            | ParsedStmt::DoesDecl { .. }
            | ParsedStmt::WillDecl { .. }
            | ParsedStmt::DeclTail { .. }
            | ParsedStmt::DeclCompound { .. }
            | ParsedStmt::SigillessDecl(_)
            | ParsedStmt::PackageHeader { .. }
            | ParsedStmt::TempDecl(_)
            | ParsedStmt::ImmediateCall { .. }
            | ParsedStmt::AttributeGroup(_)
            | ParsedStmt::AttributeColonArglist(_)
            | ParsedStmt::MethodAssignDecl(_)
            | ParsedStmt::SignatureBind { .. }
            | ParsedStmt::SignatureDecl(_)
            | ParsedStmt::Temporize(_)
            | ParsedStmt::CommaDecls(_)
            | ParsedStmt::TakeLogicalTail { .. }
            | ParsedStmt::DoneValue(_)
            | ParsedStmt::InlineProtoUse { .. }
            | ParsedStmt::CallAssignList { .. }
            | ParsedStmt::RoutineTraits(_)
            | ParsedStmt::Modified(_) => Vec::new(),
        }
    }
}

impl Modified {
    /// The modifiers, in the order they apply (condition, then loop).
    // Cost: O(1).
    pub(crate) fn modifiers(&self) -> impl Iterator<Item = &StatementModifier> {
        self.condition.iter().chain(self.loop_.iter())
    }
}

impl ParsedBlock {
    /// Where the block was written.
    // Cost: O(1).
    pub(crate) fn spans(&self) -> BlockSpans {
        BlockSpans {
            span: self.span,
            blockoid: self.blockoid,
            list: self.list,
        }
    }

    /// Whether the block was written with `->`.
    // Cost: O(1).
    pub(crate) fn is_pointy(&self) -> bool {
        !matches!(self.params, BlockParams::None)
    }
}

impl Stmt {
    /// The statement passes treat this one as: the wrapped statement of a
    /// node whose internal form is already its source form (`for`), else
    /// `self`.
    // Cost: O(1).
    pub(crate) fn peel_parsed(&self) -> &Stmt {
        match self {
            Stmt::Ast(node) => node.inner_stmt().unwrap_or(self),
            other => other,
        }
    }

    /// [`Stmt::peel_parsed`], mutably.
    // Cost: O(1).
    pub(crate) fn peel_parsed_mut(&mut self) -> &mut Stmt {
        if !matches!(self, Stmt::Ast(node) if node.inner_stmt().is_some()) {
            return self;
        }
        match self {
            Stmt::Ast(node) => node.inner_stmt_mut().expect("checked above"),
            _ => unreachable!("checked above"),
        }
    }
}

impl ParsedStmt {
    /// The declarations of a comma list as written, flattening a list that is
    /// itself one of the operands (`my $a, my $b, my $c` parses as `my $a,
    /// (my $b, my $c)`).
    // Cost: O(d), d = declarations.
    pub(crate) fn comma_operands(stmt: Stmt) -> Vec<Stmt> {
        match stmt {
            Stmt::Ast(node) => match *node {
                ParsedStmt::CommaDecls(declarations) => declarations,
                other => vec![Stmt::Ast(Box::new(other))],
            },
            other => vec![other],
        }
    }
}

impl ParsedExpr {
    /// The body of a `start { BODY }` the compiler must reject for its
    /// placeholder: kept as written, for the conversion.
    // Cost: O(1).
    pub(crate) fn start_body(body: Vec<Stmt>) -> Expr {
        Expr::Ast(Box::new(ParsedExpr::StartBody(body)))
    }
}
