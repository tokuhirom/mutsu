use super::*;

/// Collect all non-Eof token kinds from input
fn token_kinds(input: &str) -> Vec<TokenKind> {
    let mut lexer = Lexer::new(input);
    let mut kinds = Vec::new();
    loop {
        let tok = lexer.next_token();
        if matches!(tok.kind, TokenKind::Eof) {
            break;
        }
        kinds.push(tok.kind);
    }
    kinds
}

#[test]
fn test_m_slash_delim() {
    let kinds = token_kinds("m/abc/");
    assert_eq!(kinds, vec![TokenKind::Regex("abc".to_string())]);
}

#[test]
fn test_m_brace_delim() {
    let kinds = token_kinds("m{abc}");
    assert_eq!(kinds, vec![TokenKind::Regex("abc".to_string())]);
}

#[test]
fn test_m_bang_delim() {
    let kinds = token_kinds("m!b!");
    assert_eq!(kinds, vec![TokenKind::Regex("b".to_string())]);
}

#[test]
fn test_m_caret_delim() {
    let kinds = token_kinds("m^pattern^");
    assert_eq!(kinds, vec![TokenKind::Regex("pattern".to_string())]);
}

#[test]
fn test_m_hash_delim() {
    let kinds = token_kinds("m#test#");
    assert_eq!(kinds, vec![TokenKind::Regex("test".to_string())]);
}

#[test]
fn test_m_pipe_delim() {
    let kinds = token_kinds("m|foo|");
    assert_eq!(kinds, vec![TokenKind::Regex("foo".to_string())]);
}

#[test]
fn test_m_at_delim() {
    let kinds = token_kinds("m@bar@");
    assert_eq!(kinds, vec![TokenKind::Regex("bar".to_string())]);
}

#[test]
fn test_m_followed_by_alpha_is_ident() {
    let kinds = token_kinds("my");
    assert_eq!(kinds, vec![TokenKind::Ident("my".to_string())]);
}

#[test]
fn test_s_bang_delim() {
    let kinds = token_kinds("s!a!b!");
    assert_eq!(
        kinds,
        vec![TokenKind::Subst {
            pattern: "a".to_string(),
            replacement: "b".to_string(),
        }]
    );
}

#[test]
fn test_s_caret_delim() {
    let kinds = token_kinds("s^pat^repl^");
    assert_eq!(
        kinds,
        vec![TokenKind::Subst {
            pattern: "pat".to_string(),
            replacement: "repl".to_string(),
        }]
    );
}

#[test]
fn test_smartmatch_with_m_bang() {
    let kinds = token_kinds("'abc' ~~ m!b!");
    assert!(
        kinds.contains(&TokenKind::Regex("b".to_string())),
        "Expected Regex(\"b\") in tokens, got: {:?}",
        kinds
    );
}
