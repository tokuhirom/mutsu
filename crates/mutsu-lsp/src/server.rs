//! The protocol loop.
//!
//! Synchronous and single-threaded on purpose. ADR-0065 D3 drops every
//! latency-sensitive method, so there is no keystroke budget to defend and
//! nothing to overlap; parsing on the loop thread also keeps the parser's
//! thread-local caches warm, which is the exact configuration the S0 probe
//! validated for a long-lived process. A parser panic is caught inside
//! `mutsu::analysis::check` and reported as a diagnostic, so one bad document
//! cannot take the session down.

use std::error::Error;

use lsp_server::{Connection, ErrorCode, Message, Notification, Response};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument,
    Notification as NotificationTrait, PublishDiagnostics,
};
use lsp_types::{
    PositionEncodingKind, PublishDiagnosticsParams, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions, Uri,
};

use crate::diagnostics::diagnostics_for;
use crate::documents::Documents;

pub type BoxError = Box<dyn Error + Send + Sync>;

/// What this server tells the client it can do.
///
/// The list is short by design (ADR-0065 D3): an agent does not type character
/// by character, so `completion`, `semanticTokens`, `signatureHelp` and
/// `inlayHint` are absent, and document sync is full-text rather than
/// incremental.
pub fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        // UTF-16 is the one encoding every client must support. Negotiating
        // UTF-8 when offered would save a conversion; it would also make the
        // server's output depend on the client, which is the last thing a
        // consumer that never notices a bad range needs (D5).
        position_encoding: Some(PositionEncodingKind::UTF16),
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                ..Default::default()
            },
        )),
        ..Default::default()
    }
}

/// Serve one client to completion over `connection`.
pub fn run(connection: Connection) -> Result<(), BoxError> {
    let capabilities = serde_json::to_value(server_capabilities())?;
    let _initialize_params = connection.initialize(capabilities)?;

    let mut documents = Documents::default();
    for message in &connection.receiver {
        match message {
            Message::Request(request) => {
                if connection.handle_shutdown(&request)? {
                    return Ok(());
                }
                // Nothing else is implemented yet (S1). Answering
                // MethodNotFound is the honest reply, and better than silence:
                // a client that waits forever for a response it will never get
                // is a worse failure than one told plainly it asked for
                // something this server does not do.
                connection.sender.send(Message::Response(Response::new_err(
                    request.id,
                    ErrorCode::MethodNotFound as i32,
                    format!("mutsu-lsp does not implement {}", request.method),
                )))?;
            }
            Message::Response(_) => {}
            Message::Notification(notification) => {
                // A malformed notification is the client's bug, and killing the
                // session over it would lose every other open document. Report
                // it on stderr (where a client shows the server's log) and keep
                // serving.
                let method = notification.method.clone();
                if let Err(e) = handle_notification(&connection, &mut documents, notification) {
                    eprintln!("mutsu-lsp: ignoring {method}: {e}");
                }
            }
        }
    }
    Ok(())
}

fn handle_notification(
    connection: &Connection,
    documents: &mut Documents,
    notification: Notification,
) -> Result<(), BoxError> {
    match notification.method.as_str() {
        DidOpenTextDocument::METHOD => {
            let params: lsp_types::DidOpenTextDocumentParams =
                serde_json::from_value(notification.params)?;
            let opened = params.text_document;
            let uri = opened.uri;
            let version = opened.version;
            let stored = documents.open(uri.clone(), opened.text, version);
            let diagnostics = diagnostics_for(&stored.text);
            publish(connection, &uri, diagnostics, Some(version))?;
        }
        DidChangeTextDocument::METHOD => {
            let params: lsp_types::DidChangeTextDocumentParams =
                serde_json::from_value(notification.params)?;
            // Sync is full-text (D3), so the last change carries the whole
            // document. A client that ignores the negotiated sync kind and
            // sends ranged edits would land here with a partial text; taking
            // the last entry is what the protocol prescribes for FULL sync.
            let Some(change) = params.content_changes.into_iter().next_back() else {
                return Ok(());
            };
            let uri = params.text_document.uri;
            let version = params.text_document.version;
            // A change to a document the server was never told about is a
            // client protocol violation; there is nothing sensible to analyse.
            let Some(stored) = documents.replace(&uri, change.text, version) else {
                return Ok(());
            };
            let diagnostics = diagnostics_for(&stored.text);
            publish(connection, &uri, diagnostics, Some(version))?;
        }
        DidCloseTextDocument::METHOD => {
            let params: lsp_types::DidCloseTextDocumentParams =
                serde_json::from_value(notification.params)?;
            documents.close(&params.text_document.uri);
            // A closed document's diagnostics must be withdrawn explicitly, by
            // publishing an empty list; clients keep showing the last report
            // otherwise.
            publish(connection, &params.text_document.uri, Vec::new(), None)?;
        }
        _ => {}
    }
    Ok(())
}

fn publish(
    connection: &Connection,
    uri: &Uri,
    diagnostics: Vec<lsp_types::Diagnostic>,
    version: Option<i32>,
) -> Result<(), BoxError> {
    let params = PublishDiagnosticsParams {
        uri: uri.clone(),
        diagnostics,
        version,
    };
    connection
        .sender
        .send(Message::Notification(Notification::new(
            PublishDiagnostics::METHOD.to_string(),
            params,
        )))?;
    Ok(())
}
