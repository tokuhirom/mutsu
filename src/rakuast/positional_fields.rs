//! Accessor names for the model classes with positional payloads.

use super::RakuAstClass;

/// The accessor over a single positional field. Named lookup runs first,
/// so a class with a named field of the same name is unaffected.
pub(super) fn positional_accessor(class: RakuAstClass) -> Option<&'static str> {
    use RakuAstClass::*;
    Some(match class {
        IntLiteral | NumLiteral | RatLiteral | VersionLiteral | Constant | Literal
        | ComplexLiteral | StrLiteral => "value",
        FunctionInfix => "function",
        FakeSignature => "signature",
        Infix | Prefix | Feed | FlipFlop | Mixin => "operator",
        VarAttribute => "name",
        VarLexical | VarDynamic | VarDoc => "name",
        VarCompilerLookup => "name",
        VarCompilerFile => "file",
        VarCompilerLine => "line",
        Label => "name",
        NamePartSimple => "name",
        NamePartExpression => "expr",
        TermName | TermNamed | ParameterTargetTerm => "name",
        TermTopicCall => "call",
        Blockoid => "statement-list",
        VarDeclarationPlaceholderPositional | VarDeclarationPlaceholderNamed => "lexical-name",
        InitializerAssign | InitializerBind => "expression",
        InitializerCallAssign => "postfixish",
        MetaInfixAssign => "infix",
        MetaInfixNegate | BracketedInfix => "infix",
        RegexSequentialConjunction => "branches",
        RegexConjunction => "branches",
        RegexBackReferencePositional => "index",
        RegexBackReferenceNamed => "name",
        RegexStatement => "statement",
        VarPositionalCapture => "index",
        VarNamedCapture => "index",
        MetaPostfixHyper => "postfix",
        MetaPrefixHyper => "prefix",
        TermCapture => "source",
        QuoteWordsAtom => "atom",
        MetaInfixReverse => "infix",
        MetaInfixCross => "infix",
        MetaInfixZip => "infix",
        TypeSimple | TypeSetting | TypeCapture => "name",
        TraitReturns | TraitOf | TraitDoes | TraitHides => "type",
        TraitHandles => "term",
        RegexLiteral => "text",
        RegexQuote => "quoted",
        RegexSequence => "terms",
        RegexAssertionCharClass => "elements",
        RegexCharClassEnumerationElementCharacter => "character",
        RegexAlternation | RegexSequentialAlternation => "branches",
        RegexGroup | RegexCapturingGroup | RegexWithWhitespace => "regex",
        RegexBlock => "block",
        StatementLanguageVersion => "version",
        StatementPrefixReact | StatementPrefixSupply => "blorst",
        CircumfixHashComposer => "expression",
        CircumfixArrayComposer | CircumfixParentheses => "semilist",
        ContextualizerHash | ContextualizerItem | ContextualizerList => "target",
        ColonPairTrue | ColonPairFalse => "key",
        StatementModifierFor
        | StatementModifierWhile
        | StatementModifierUntil
        | StatementModifierGiven
        | StatementModifierIf
        | StatementModifierUnless
        | StatementModifierWith
        | StatementModifierWhen
        | StatementModifierWithout => "expression",
        // Every statement prefix wraps its block-or-statement ("blorst")
        // positionally, and rakudo names it `.blorst` (#9761).
        StatementPrefixDo
        | StatementPrefixTry
        | StatementPrefixGather
        | StatementPrefixEager
        | StatementPrefixOnce
        | StatementPrefixStart
        | StatementPrefixQuietly
        | StatementPrefixSink
        | StatementPrefixLazy
        | StatementPrefixHyper
        | StatementPrefixRace
        | StatementPrefixPhaserBegin
        | StatementPrefixPhaserCheck
        | StatementPrefixPhaserClose
        | StatementPrefixPhaserEnd
        | StatementPrefixPhaserEnter
        | StatementPrefixPhaserFirst
        | StatementPrefixPhaserInit
        | StatementPrefixPhaserKeep
        | StatementPrefixPhaserLast
        | StatementPrefixPhaserLeave
        | StatementPrefixPhaserNext
        | StatementPrefixPhaserPost
        | StatementPrefixPhaserPre
        | StatementPrefixPhaserQuit
        | StatementPrefixPhaserUndo => "blorst",
        _ => return None,
    })
}
