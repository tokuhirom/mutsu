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

use lsp_server::{Connection, ErrorCode, Message, Notification, Request, Response};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument,
    Notification as NotificationTrait, PublishDiagnostics,
};
use lsp_types::request::{
    DocumentSymbolRequest, GotoDefinition, Request as RequestTrait, WorkspaceSymbolRequest,
};
use lsp_types::{
    DocumentSymbolResponse, GotoDefinitionResponse, Location, OneOf, PositionEncodingKind,
    PublishDiagnosticsParams, ServerCapabilities, SymbolInformation, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions, Uri, WorkspaceSymbolResponse,
};

use crate::diagnostics::diagnostics_for;
use crate::documents::Documents;
use crate::symbols;
use crate::workspace::Workspace;

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
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
        definition_provider: Some(OneOf::Left(true)),
        ..Default::default()
    }
}

/// Serve one client to completion over `connection`.
pub fn run(connection: Connection) -> Result<(), BoxError> {
    let capabilities = serde_json::to_value(server_capabilities())?;
    let initialize_params = connection.initialize(capabilities)?;

    let mut documents = Documents::default();
    let mut workspace = Workspace::from_initialize_params(&initialize_params);
    for message in &connection.receiver {
        match message {
            Message::Request(request) => {
                if connection.handle_shutdown(&request)? {
                    return Ok(());
                }
                let response = handle_request(&documents, &mut workspace, request);
                connection.sender.send(Message::Response(response))?;
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

/// Answer one request.
///
/// A method this server does not implement is answered with `MethodNotFound`
/// rather than ignored: a client waiting forever for a response it will never
/// get is a worse failure than one told plainly.
fn handle_request(documents: &Documents, workspace: &mut Workspace, request: Request) -> Response {
    let id = request.id.clone();
    let result = match request.method.as_str() {
        DocumentSymbolRequest::METHOD => document_symbol(documents, request),
        WorkspaceSymbolRequest::METHOD => workspace_symbol(workspace, request),
        GotoDefinition::METHOD => definition(documents, workspace, request),
        method => {
            return Response::new_err(
                id,
                ErrorCode::MethodNotFound as i32,
                format!("mutsu-lsp does not implement {method}"),
            );
        }
    };
    match result {
        Ok(value) => Response::new_ok(id, value),
        // Malformed params are the client's bug. Reporting `InvalidParams`
        // keeps the session going, where a hard error would end it.
        Err(e) => Response::new_err(id, ErrorCode::InvalidParams as i32, e.to_string()),
    }
}

fn document_symbol(documents: &Documents, request: Request) -> Result<serde_json::Value, BoxError> {
    let params: lsp_types::DocumentSymbolParams = serde_json::from_value(request.params)?;
    let outline = documents
        .get(&params.text_document.uri)
        .map(|doc| symbols::document_symbols(&doc.text))
        .unwrap_or_default();
    Ok(serde_json::to_value(DocumentSymbolResponse::Nested(
        outline,
    ))?)
}

fn workspace_symbol(
    workspace: &mut Workspace,
    request: Request,
) -> Result<serde_json::Value, BoxError> {
    let params: lsp_types::WorkspaceSymbolParams = serde_json::from_value(request.params)?;
    let mut found = Vec::new();
    for path in workspace.files() {
        let Some(uri) = uri_of_path(&path) else {
            continue;
        };
        let Some(text) = workspace.text_of(&path) else {
            continue;
        };
        for (symbol, container, range) in symbols::flat_symbols(text) {
            if !symbols::matches_query(&symbol.name, &params.query) {
                continue;
            }
            #[allow(deprecated)] // `SymbolInformation::deprecated` is not optional
            found.push(SymbolInformation {
                name: symbol.name,
                kind: symbols::lsp_kind(symbol.kind),
                tags: None,
                deprecated: None,
                location: Location {
                    uri: uri.clone(),
                    range,
                },
                container_name: container,
            });
        }
    }
    Ok(serde_json::to_value(WorkspaceSymbolResponse::Flat(found))?)
}

fn definition(
    documents: &Documents,
    workspace: &mut Workspace,
    request: Request,
) -> Result<serde_json::Value, BoxError> {
    let params: lsp_types::GotoDefinitionParams = serde_json::from_value(request.params)?;
    let uri = params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    let Some(document) = documents.get(&uri) else {
        return Ok(serde_json::Value::Null);
    };
    // What the caret is on comes from the document text, not the AST: mutsu has
    // positions for declarations (the `SetLine` markers) but none for
    // references, and an identifier is a lexical notion that needs no parse.
    let Some(name) = crate::positions::identifier_at(&document.text, position) else {
        return Ok(serde_json::Value::Null);
    };

    // The open document first: a definition in the file being edited is both the
    // likeliest answer and the freshest, since the workspace copy on disk may be
    // older than what the client is holding.
    if let Some(location) = symbols::definition_in(&uri, &document.text, &name) {
        return Ok(serde_json::to_value(GotoDefinitionResponse::Scalar(
            location,
        ))?);
    }
    for path in workspace.files() {
        let Some(file_uri) = uri_of_path(&path) else {
            continue;
        };
        if file_uri == uri {
            continue;
        }
        let Some(text) = workspace.text_of(&path) else {
            continue;
        };
        if let Some(location) = symbols::definition_in(&file_uri, text, &name) {
            return Ok(serde_json::to_value(GotoDefinitionResponse::Scalar(
                location,
            ))?);
        }
    }
    Ok(serde_json::Value::Null)
}

fn uri_of_path(path: &std::path::Path) -> Option<Uri> {
    use std::str::FromStr;
    Uri::from_str(&format!("file://{}", path.to_str()?)).ok()
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
