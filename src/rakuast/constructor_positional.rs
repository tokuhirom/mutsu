//! Single-operand constructors, including conditional and loop modifiers.

use super::RakuAstClass;

/// The class for a single-positional-argument constructor, or `None`.
pub(super) fn single_positional_class(class_name: &str, method: &str) -> Option<RakuAstClass> {
    Some(match (class_name, method) {
        ("RakuAST::StatementModifier::If", "new") => RakuAstClass::StatementModifierIf,
        ("RakuAST::StatementModifier::Unless", "new") => RakuAstClass::StatementModifierUnless,
        ("RakuAST::StatementModifier::With", "new") => RakuAstClass::StatementModifierWith,
        ("RakuAST::StatementModifier::Without", "new") => RakuAstClass::StatementModifierWithout,
        ("RakuAST::StatementModifier::When", "new") => RakuAstClass::StatementModifierWhen,
        ("RakuAST::StatementModifier::For", "new") => RakuAstClass::StatementModifierFor,
        ("RakuAST::StatementModifier::Given", "new") => RakuAstClass::StatementModifierGiven,
        ("RakuAST::StatementModifier::While", "new") => RakuAstClass::StatementModifierWhile,
        ("RakuAST::StatementModifier::Until", "new") => RakuAstClass::StatementModifierUntil,
        ("RakuAST::IntLiteral", "new") => RakuAstClass::IntLiteral,
        ("RakuAST::NumLiteral", "new") => RakuAstClass::NumLiteral,
        ("RakuAST::RatLiteral", "new") => RakuAstClass::RatLiteral,
        ("RakuAST::VersionLiteral", "new") => RakuAstClass::VersionLiteral,
        ("RakuAST::ComplexLiteral", "new") => RakuAstClass::ComplexLiteral,
        ("RakuAST::StrLiteral", "new") => RakuAstClass::StrLiteral,
        ("RakuAST::Name", "from-identifier") => RakuAstClass::Name,
        ("RakuAST::Trait::Does", "new") => RakuAstClass::TraitDoes,
        ("RakuAST::Trait::Hides", "new") => RakuAstClass::TraitHides,
        ("RakuAST::Name::Part::Simple", "new") => RakuAstClass::NamePartSimple,
        ("RakuAST::Name::Part::Expression", "new") => RakuAstClass::NamePartExpression,
        ("RakuAST::Term::Name", "new") => RakuAstClass::TermName,
        ("RakuAST::Term::Named", "new") => RakuAstClass::TermNamed,
        ("RakuAST::Term::TopicCall", "new") => RakuAstClass::TermTopicCall,
        ("RakuAST::ParameterTarget::Term", "new") => RakuAstClass::ParameterTargetTerm,
        ("RakuAST::Term::Enum", "from-identifier") => RakuAstClass::TermEnum,
        ("RakuAST::Infix", "new") => RakuAstClass::Infix,
        ("RakuAST::FunctionInfix", "new") => RakuAstClass::FunctionInfix,
        ("RakuAST::MetaInfix::Assign", "new") => RakuAstClass::MetaInfixAssign,
        ("RakuAST::MetaInfix::Negate", "new") => RakuAstClass::MetaInfixNegate,
        ("RakuAST::BracketedInfix", "new") => RakuAstClass::BracketedInfix,
        ("RakuAST::Regex::SequentialConjunction", "new") => {
            RakuAstClass::RegexSequentialConjunction
        }
        ("RakuAST::Regex::Conjunction", "new") => RakuAstClass::RegexConjunction,
        ("RakuAST::Regex::BacktrackModifiedAtom", "new") => {
            RakuAstClass::RegexBacktrackModifiedAtom
        }
        ("RakuAST::Regex::Quantifier::BlockRange", "new") => {
            RakuAstClass::RegexQuantifierBlockRange
        }
        ("RakuAST::Regex::Assertion::Recurse", "new") => RakuAstClass::RegexAssertionRecurse,
        ("RakuAST::Regex::BackReference::Positional", "new") => {
            RakuAstClass::RegexBackReferencePositional
        }
        ("RakuAST::Regex::BackReference::Named", "new") => RakuAstClass::RegexBackReferenceNamed,
        ("RakuAST::Regex::Statement", "new") => RakuAstClass::RegexStatement,
        ("RakuAST::Regex::Nested", "new") => RakuAstClass::RegexNested,
        ("RakuAST::Var::PositionalCapture", "new") => RakuAstClass::VarPositionalCapture,
        ("RakuAST::ColonPair::Number", "new") => RakuAstClass::ColonPairNumber,
        ("RakuAST::Transliteration", "new") => RakuAstClass::Transliteration,
        ("RakuAST::Substitution", "new") => RakuAstClass::Substitution,
        ("RakuAST::Var::NamedCapture", "new") => RakuAstClass::VarNamedCapture,
        ("RakuAST::Call::NameAsMethod", "new") => RakuAstClass::CallNameAsMethod,
        ("RakuAST::Call::TermAsMethod", "new") => RakuAstClass::CallTermAsMethod,
        ("RakuAST::FlipFlop", "new") => RakuAstClass::FlipFlop,
        ("RakuAST::Feed", "new") => RakuAstClass::Feed,
        ("RakuAST::StatementPrefix::Eager", "new") => RakuAstClass::StatementPrefixEager,
        ("RakuAST::Term::Capture", "new") => RakuAstClass::TermCapture,
        ("RakuAST::QuoteWordsAtom", "new") => RakuAstClass::QuoteWordsAtom,
        ("RakuAST::MetaInfix::Reverse", "new") => RakuAstClass::MetaInfixReverse,
        ("RakuAST::MetaInfix::Cross", "new") => RakuAstClass::MetaInfixCross,
        ("RakuAST::MetaInfix::Zip", "new") => RakuAstClass::MetaInfixZip,
        ("RakuAST::MetaPrefix::Hyper", "new") => RakuAstClass::MetaPrefixHyper,
        ("RakuAST::Prefix", "new") => RakuAstClass::Prefix,
        ("RakuAST::Var::Lexical", "new") => RakuAstClass::VarLexical,
        ("RakuAST::Var::Compiler::Lookup", "new") => RakuAstClass::VarCompilerLookup,
        ("RakuAST::Var::Compiler::File", "new") => RakuAstClass::VarCompilerFile,
        ("RakuAST::Var::Compiler::Line", "new") => RakuAstClass::VarCompilerLine,
        ("RakuAST::Var::Dynamic", "new") => RakuAstClass::VarDynamic,
        ("RakuAST::Circumfix::Parentheses", "new") => RakuAstClass::CircumfixParentheses,
        ("RakuAST::Circumfix::ArrayComposer", "new") => RakuAstClass::CircumfixArrayComposer,
        ("RakuAST::VarDeclaration::Placeholder::Positional", "new") => {
            RakuAstClass::VarDeclarationPlaceholderPositional
        }
        ("RakuAST::VarDeclaration::Placeholder::Named", "new") => {
            RakuAstClass::VarDeclarationPlaceholderNamed
        }
        ("RakuAST::Initializer::Assign", "new") => RakuAstClass::InitializerAssign,
        ("RakuAST::Initializer::CallAssign", "new") => RakuAstClass::InitializerCallAssign,
        ("RakuAST::Initializer::Bind", "new") => RakuAstClass::InitializerBind,
        ("RakuAST::Type::Simple", "new") => RakuAstClass::TypeSimple,
        ("RakuAST::Type::Setting", "new") => RakuAstClass::TypeSetting,
        ("RakuAST::Type::Definedness", "new") => RakuAstClass::TypeDefinedness,
        ("RakuAST::Type::AnyDefinedness", "new") => RakuAstClass::TypeAnyDefinedness,
        ("RakuAST::Type::Coercion", "new") => RakuAstClass::TypeCoercion,
        ("RakuAST::Type::Capture", "new") => RakuAstClass::TypeCapture,
        ("RakuAST::Trait::Returns", "new") => RakuAstClass::TraitReturns,
        ("RakuAST::Trait::Of", "new") => RakuAstClass::TraitOf,
        ("RakuAST::Regex::Literal", "new") => RakuAstClass::RegexLiteral,
        ("RakuAST::Regex::Quote", "new") => RakuAstClass::RegexQuote,
        ("RakuAST::Regex::Group", "new") => RakuAstClass::RegexGroup,
        ("RakuAST::Regex::CapturingGroup", "new") => RakuAstClass::RegexCapturingGroup,
        ("RakuAST::Regex::WithWhitespace", "new") => RakuAstClass::RegexWithWhitespace,
        ("RakuAST::Regex::Block", "new") => RakuAstClass::RegexBlock,
        ("RakuAST::ColonPair::True", "new") => RakuAstClass::ColonPairTrue,
        ("RakuAST::ColonPair::False", "new") => RakuAstClass::ColonPairFalse,
        // Each phaser kind wraps its blorst positionally, exactly as the read
        // direction (`convert.rs`) builds it.
        ("RakuAST::StatementPrefix::Phaser::Begin", "new") => {
            RakuAstClass::StatementPrefixPhaserBegin
        }
        ("RakuAST::StatementPrefix::Do", "new") => RakuAstClass::StatementPrefixDo,
        ("RakuAST::StatementPrefix::Try", "new") => RakuAstClass::StatementPrefixTry,
        ("RakuAST::StatementPrefix::Gather", "new") => RakuAstClass::StatementPrefixGather,
        ("RakuAST::StatementPrefix::Once", "new") => RakuAstClass::StatementPrefixOnce,
        ("RakuAST::StatementPrefix::Start", "new") => RakuAstClass::StatementPrefixStart,
        ("RakuAST::StatementPrefix::Quietly", "new") => RakuAstClass::StatementPrefixQuietly,
        ("RakuAST::StatementPrefix::Sink", "new") => RakuAstClass::StatementPrefixSink,
        ("RakuAST::StatementPrefix::Lazy", "new") => RakuAstClass::StatementPrefixLazy,
        ("RakuAST::StatementPrefix::Hyper", "new") => RakuAstClass::StatementPrefixHyper,
        ("RakuAST::StatementPrefix::Race", "new") => RakuAstClass::StatementPrefixRace,
        ("RakuAST::Mixin", "new") => RakuAstClass::Mixin,
        ("RakuAST::Var::Attribute", "new") => RakuAstClass::VarAttribute,
        ("RakuAST::StatementPrefix::React", "new") => RakuAstClass::StatementPrefixReact,
        ("RakuAST::StatementPrefix::Supply", "new") => RakuAstClass::StatementPrefixSupply,
        ("RakuAST::StatementPrefix::Phaser::Check", "new") => {
            RakuAstClass::StatementPrefixPhaserCheck
        }
        ("RakuAST::StatementPrefix::Phaser::Init", "new") => {
            RakuAstClass::StatementPrefixPhaserInit
        }
        ("RakuAST::StatementPrefix::Phaser::End", "new") => RakuAstClass::StatementPrefixPhaserEnd,
        ("RakuAST::StatementPrefix::Phaser::Enter", "new") => {
            RakuAstClass::StatementPrefixPhaserEnter
        }
        ("RakuAST::StatementPrefix::Phaser::Leave", "new") => {
            RakuAstClass::StatementPrefixPhaserLeave
        }
        ("RakuAST::StatementPrefix::Phaser::Keep", "new") => {
            RakuAstClass::StatementPrefixPhaserKeep
        }
        ("RakuAST::StatementPrefix::Phaser::Undo", "new") => {
            RakuAstClass::StatementPrefixPhaserUndo
        }
        ("RakuAST::StatementPrefix::Phaser::First", "new") => {
            RakuAstClass::StatementPrefixPhaserFirst
        }
        ("RakuAST::StatementPrefix::Phaser::Next", "new") => {
            RakuAstClass::StatementPrefixPhaserNext
        }
        ("RakuAST::StatementPrefix::Phaser::Last", "new") => {
            RakuAstClass::StatementPrefixPhaserLast
        }
        ("RakuAST::StatementPrefix::Phaser::Quit", "new") => {
            RakuAstClass::StatementPrefixPhaserQuit
        }
        ("RakuAST::StatementPrefix::Phaser::Close", "new") => {
            RakuAstClass::StatementPrefixPhaserClose
        }
        _ => return None,
    })
}
