//! The mutsu language server (ADR-0065).
//!
//! A language server *for mutsu*: it reports what mutsu makes of a document,
//! and its intended consumer is an AI agent writing Raku that will run on
//! mutsu. That consumer shapes every decision in here — see
//! `docs/adr/0065-language-server-targets-ai-agents.md`. The two that show up
//! most in this code are D3 (only the methods an agent consumes are
//! implemented, which removes incremental sync, completion and semantic tokens)
//! and D5 (the message matters more than the range, and the range must be
//! pinned by tests because an agent will never report that it is wrong).

pub mod diagnostics;
pub mod documents;
pub mod positions;
pub mod server;
