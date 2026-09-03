//! End-to-end protocol tests: drive the real loop over an in-memory transport.
//!
//! `lsp_server::Connection::memory()` gives a client/server channel pair, so
//! these exercise the same `server::run` a real editor talks to — the
//! initialize handshake, document sync, diagnostic publication and shutdown —
//! without a subprocess.
//!
//! These are also where ADR-0065 D5's mandate lands: the intended consumer is an
//! AI agent, which absorbs an off-by-one range silently and never reports it, so
//! positions must be pinned by assertions from the first slice.

use std::str::FromStr;
use std::time::Duration;

use lsp_server::{Connection, Message, Notification, Request, RequestId, Response};
use lsp_types::{DiagnosticSeverity, Position, PublishDiagnosticsParams, Uri};

const TIMEOUT: Duration = Duration::from_secs(30);

fn uri(s: &str) -> Uri {
    Uri::from_str(s).expect("valid uri")
}

/// A client driving `server::run` on a worker thread.
struct Client {
    connection: Connection,
    server: Option<std::thread::JoinHandle<Result<(), String>>>,
    next_id: i32,
}

impl Client {
    /// Connect and complete the initialize handshake with no workspace.
    fn start() -> Client {
        Client::start_in(serde_json::json!({ "capabilities": {} }))
    }

    /// Connect with `root` as the workspace root.
    fn start_in_workspace(root: &std::path::Path) -> Client {
        Client::start_in(serde_json::json!({
            "capabilities": {},
            "rootUri": format!("file://{}", root.display()),
        }))
    }

    fn start_in(initialize_params: serde_json::Value) -> Client {
        let (server_connection, client_connection) = Connection::memory();
        // The same stack the binary gives the loop. mutsu's parser is deeply
        // recursive enough that this is not a formality: on a default stack it
        // overflows — and *aborts*, which no `catch_unwind` can rescue — on a
        // document with about fifty nested parentheses.
        let server = std::thread::Builder::new()
            .stack_size(mutsu_lsp::ANALYSIS_STACK_SIZE)
            .spawn(move || mutsu_lsp::server::run(server_connection).map_err(|e| e.to_string()))
            .expect("spawn the server thread");
        let mut client = Client {
            connection: client_connection,
            server: Some(server),
            next_id: 0,
        };

        let id = client.request("initialize", initialize_params);
        let result = client
            .recv_response(id)
            .response_result
            .expect("initialize must succeed");
        let capabilities = &result["capabilities"];
        assert_eq!(
            capabilities["positionEncoding"], "utf-16",
            "the server must announce the encoding its ranges are in"
        );
        assert_eq!(
            capabilities["textDocumentSync"]["change"], 1,
            "full-document sync (ADR-0065 D3 rejects incremental sync)"
        );
        assert!(
            capabilities.get("completionProvider").is_none(),
            "completion is out of scope (D3) and must not be advertised"
        );
        assert!(
            capabilities.get("semanticTokensProvider").is_none(),
            "semantic tokens are out of scope (D3) and must not be advertised"
        );

        client.notify("initialized", serde_json::json!({}));
        client
    }

    fn request(&mut self, method: &str, params: serde_json::Value) -> RequestId {
        let id = RequestId::from(self.next_id);
        self.next_id += 1;
        self.connection
            .sender
            .send(Message::Request(Request {
                id: id.clone(),
                method: method.to_string(),
                params,
            }))
            .expect("send request");
        id
    }

    fn notify(&self, method: &str, params: serde_json::Value) {
        self.connection
            .sender
            .send(Message::Notification(Notification {
                method: method.to_string(),
                params,
            }))
            .expect("send notification");
    }

    fn recv(&self) -> Message {
        self.connection
            .receiver
            .recv_timeout(TIMEOUT)
            .expect("server must answer")
    }

    fn recv_response(&self, expected: RequestId) -> Response {
        match self.recv() {
            Message::Response(r) => {
                assert_eq!(r.id, expected);
                r
            }
            other => panic!("expected a response, got {other:?}"),
        }
    }

    /// Read the next `textDocument/publishDiagnostics` notification.
    fn recv_diagnostics(&self) -> PublishDiagnosticsParams {
        match self.recv() {
            Message::Notification(n) => {
                assert_eq!(n.method, "textDocument/publishDiagnostics");
                serde_json::from_value(n.params).expect("diagnostics params")
            }
            other => panic!("expected publishDiagnostics, got {other:?}"),
        }
    }

    fn open(&self, path: &str, text: &str, version: i32) -> PublishDiagnosticsParams {
        self.notify(
            "textDocument/didOpen",
            serde_json::json!({
                "textDocument": {
                    "uri": path,
                    "languageId": "raku",
                    "version": version,
                    "text": text,
                }
            }),
        );
        self.recv_diagnostics()
    }

    fn change(&self, path: &str, text: &str, version: i32) -> PublishDiagnosticsParams {
        self.notify(
            "textDocument/didChange",
            serde_json::json!({
                "textDocument": { "uri": path, "version": version },
                "contentChanges": [{ "text": text }],
            }),
        );
        self.recv_diagnostics()
    }

    fn close(&self, path: &str) -> PublishDiagnosticsParams {
        self.notify(
            "textDocument/didClose",
            serde_json::json!({ "textDocument": { "uri": path } }),
        );
        self.recv_diagnostics()
    }

    fn shutdown(mut self) {
        let id = self.request("shutdown", serde_json::Value::Null);
        self.recv_response(id)
            .response_result
            .expect("shutdown must succeed");
        self.notify("exit", serde_json::Value::Null);
        let server = self.server.take().expect("server thread");
        server
            .join()
            .expect("the server thread must not panic")
            .expect("the server must exit cleanly");
    }
}

impl Drop for Client {
    fn drop(&mut self) {
        // A test that fails mid-way must not wedge the harness waiting on a
        // server thread that is still blocked on `recv`.
        if let Some(server) = self.server.take() {
            drop(std::mem::replace(
                &mut self.connection,
                Connection::memory().0,
            ));
            let _ = server.join();
        }
    }
}

#[test]
fn opening_a_broken_document_reports_where_it_broke() {
    let client = Client::start();
    let path = "file:///tmp/broken.raku";

    let published = client.open(path, "my $x = 1;\n}\nsay $x;\n", 1);
    assert_eq!(published.uri, uri(path));
    assert_eq!(published.version, Some(1));
    assert_eq!(
        published.diagnostics.len(),
        1,
        "{:#?}",
        published.diagnostics
    );

    let d = &published.diagnostics[0];
    assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
    assert_eq!(d.source.as_deref(), Some("mutsu"));
    assert_eq!(
        d.range.start,
        Position {
            line: 1,
            character: 0
        },
        "the stray brace opens line 2 (0-based line 1)"
    );
    assert!(
        !d.message.is_empty(),
        "an agent acts on the message, so it must never be blank"
    );

    client.shutdown();
}

#[test]
fn fixing_a_document_withdraws_the_diagnostic() {
    let client = Client::start();
    let path = "file:///tmp/fixed.raku";

    let broken = client.open(path, "my $x = 1;\n}\n", 1);
    assert_eq!(broken.diagnostics.len(), 1);

    let fixed = client.change(path, "my $x = 1;\nsay $x;\n", 2);
    assert!(
        fixed.diagnostics.is_empty(),
        "a clean document must publish an empty list, not silence: {:#?}",
        fixed.diagnostics
    );
    assert_eq!(fixed.version, Some(2));

    client.shutdown();
}

#[test]
fn closing_a_document_withdraws_its_diagnostics() {
    let client = Client::start();
    let path = "file:///tmp/closed.raku";

    assert_eq!(client.open(path, "}\n", 1).diagnostics.len(), 1);
    let cleared = client.close(path);
    assert!(cleared.diagnostics.is_empty());
    assert_eq!(cleared.version, None);

    client.shutdown();
}

#[test]
fn diagnostic_columns_are_utf16_offsets() {
    let client = Client::start();
    let path = "file:///tmp/unicode.raku";

    // Four astral-plane characters ahead of the error: 4 characters, but 8
    // UTF-16 code units. A server that reported characters would say 20 here.
    let text = "my $s = '🐪🐪🐪🐪';\nmy $t = '🐪🐪🐪🐪' }\n";
    let published = client.open(path, text, 1);
    assert_eq!(
        published.diagnostics.len(),
        1,
        "{:#?}",
        published.diagnostics
    );

    // mutsu reports the stray `}` at line 2, column 16 — the 16th *character*
    // of `my $t = '<4 camels>' }`. In UTF-16 that is offset 19: nine BMP
    // characters (`my $t = '`), then four astral camels at two code units each
    // (8), then `' ` (2). A server that passed the character column straight
    // through would say 15, and its consumer would never notice.
    let d = &published.diagnostics[0];
    assert_eq!(
        d.range.start,
        Position {
            line: 1,
            character: 19
        },
        "message was: {}",
        d.message
    );
    let line = text.lines().nth(1).expect("second line");
    let utf16_len: u32 = line.chars().map(|c| c.len_utf16() as u32).sum();
    assert_eq!(
        d.range.end.character, utf16_len,
        "the range runs to end of line"
    );
    assert!(
        (line.chars().count() as u32) < d.range.start.character,
        "the whole point: the UTF-16 offset exceeds what a character count would give"
    );

    client.shutdown();
}

/// ADR-0065 S3: a document under edit is broken most of the time, and a report
/// that goes quiet after the first failure hides everything below it.
#[test]
fn every_failure_in_the_document_reaches_the_client() {
    let client = Client::start();
    let path = "file:///tmp/two-errors.raku";

    let published = client.open(
        path,
        "say 1;\nsay $c.f (1, 2);\nsay 2;\nsay $d.g (3, 4);\nsay 3;\n",
        1,
    );
    let lines: Vec<u32> = published
        .diagnostics
        .iter()
        .filter(|d| d.severity == Some(DiagnosticSeverity::ERROR))
        .map(|d| d.range.start.line)
        .collect();
    assert_eq!(lines, vec![1, 3], "{:#?}", published.diagnostics);

    client.shutdown();
}

#[test]
fn an_unimplemented_request_is_answered_rather_than_ignored() {
    let mut client = Client::start();
    client.open("file:///tmp/hover.raku", "say 1;\n", 1);

    let id = client.request(
        "textDocument/hover",
        serde_json::json!({
            "textDocument": { "uri": "file:///tmp/hover.raku" },
            "position": { "line": 0, "character": 0 },
        }),
    );
    let error = client
        .recv_response(id)
        .response_result
        .expect_err("an unimplemented method must error");
    assert_eq!(error.code, lsp_server::ErrorCode::MethodNotFound as i32);
    assert!(error.message.contains("textDocument/hover"), "{error:?}");

    client.shutdown();
}

#[test]
fn the_session_survives_a_document_that_breaks_the_parser() {
    let client = Client::start();
    let path = "file:///tmp/hostile.raku";

    // Whatever these do to the parser, the server must still be answering
    // afterwards: a resident process cannot die on one bad document.
    let _ = client.open(path, "}}}}}}}}}}\n", 1);
    for (version, text) in [
        (2, "sub { { { { {\n"),
        (3, "my $x = '\n"),
        (4, "\u{0}\u{1}\u{2}\n"),
        (5, ""),
    ] {
        let _ = client.change(path, text, version);
    }
    // The server is still there and still analysing.
    let published = client.change(path, "my $x = 1;\nsay $x;\n", 6);
    assert!(
        published.diagnostics.is_empty(),
        "{:#?}",
        published.diagnostics
    );

    client.shutdown();
}

/// A stack overflow aborts the process, so `mutsu::analysis::check`'s
/// panic-catching cannot turn one into a diagnostic the way it does an ordinary
/// panic. The server therefore runs on the same deep stack the interpreter's own
/// CLI uses. Measured on a debug build: with an 8 MB stack this document
/// overflows at about fifty nested parentheses; with the analysis stack a
/// thousand are fine.
///
/// If the big stack is ever removed, this test does not fail politely — it
/// aborts the test binary, which is exactly the visibility the defect deserves.
#[test]
fn a_deeply_nested_document_does_not_take_the_server_down() {
    let client = Client::start();
    let path = "file:///tmp/deep.raku";

    let depth = 200;
    let text = format!(
        "my $x = {}1{};\nsay $x;\n",
        "(".repeat(depth),
        ")".repeat(depth)
    );
    let published = client.open(path, &text, 1);
    assert!(
        published.diagnostics.is_empty(),
        "{depth} nested parentheses are valid Raku: {:#?}",
        published.diagnostics
    );

    // Still serving afterwards.
    let after = client.change(path, "say 1;\n", 2);
    assert!(after.diagnostics.is_empty());

    client.shutdown();
}

/// ADR-0065 D3 lists `documentSymbol` as in scope because it gives an agent an
/// exact answer where it would otherwise grep — and grep's false positives (a
/// mention in a comment, an unrelated same-named method) are what make it a poor
/// substitute.
#[test]
fn the_outline_of_a_document_is_nested_and_placed() {
    let mut client = Client::start();
    let path = "file:///tmp/outline.raku";
    client.open(
        path,
        "class Foo {\n    has $.x;\n    method bar() { 1 }\n}\nsub baz() { 2 }\n",
        1,
    );

    let id = client.request(
        "textDocument/documentSymbol",
        serde_json::json!({ "textDocument": { "uri": path } }),
    );
    let result = client
        .recv_response(id)
        .response_result
        .expect("documentSymbol must succeed");
    let outline = result.as_array().expect("a nested outline");
    assert_eq!(outline.len(), 2, "{result:#}");
    assert_eq!(outline[0]["name"], "Foo");
    assert_eq!(outline[0]["detail"], "class");
    assert_eq!(outline[0]["selectionRange"]["start"]["line"], 0);
    let children = outline[0]["children"].as_array().expect("class members");
    assert_eq!(children.len(), 2);
    assert_eq!(children[1]["name"], "bar");
    assert_eq!(
        children[1]["selectionRange"]["start"],
        serde_json::json!({ "line": 2, "character": 11 }),
        "the selection range must land on the name, which is where a client puts the caret"
    );
    assert_eq!(outline[1]["name"], "baz");

    client.shutdown();
}

/// The outline must survive a document that does not parse: that is the state a
/// file being edited is in most of the time (S3).
#[test]
fn the_outline_survives_a_broken_document() {
    let mut client = Client::start();
    let path = "file:///tmp/broken-outline.raku";
    client.open(
        path,
        "sub good() { 1 }\nsay $c.f (1, 2);\nclass Later { }\n",
        1,
    );

    let id = client.request(
        "textDocument/documentSymbol",
        serde_json::json!({ "textDocument": { "uri": path } }),
    );
    let result = client
        .recv_response(id)
        .response_result
        .expect("documentSymbol must succeed");
    let names: Vec<&str> = result
        .as_array()
        .expect("outline")
        .iter()
        .filter_map(|s| s["name"].as_str())
        .collect();
    assert!(names.contains(&"good"), "{result:#}");
    assert!(names.contains(&"Later"), "{result:#}");

    client.shutdown();
}

#[test]
fn definition_finds_a_declaration_in_the_open_document() {
    let mut client = Client::start();
    let path = "file:///tmp/definition.raku";
    client.open(path, "sub frobnicate() { 1 }\nsay frobnicate();\n", 1);

    // The caret sits inside `frobnicate` on the second line.
    let id = client.request(
        "textDocument/definition",
        serde_json::json!({
            "textDocument": { "uri": path },
            "position": { "line": 1, "character": 8 },
        }),
    );
    let result = client
        .recv_response(id)
        .response_result
        .expect("definition must succeed");
    assert_eq!(result["uri"], path, "{result:#}");
    assert_eq!(result["range"]["start"]["line"], 0);
    assert_eq!(
        result["range"]["start"]["character"], 4,
        "the range covers the name, not the whole declaration line"
    );

    client.shutdown();
}

#[test]
fn definition_on_something_undeclared_answers_null_rather_than_erroring() {
    let mut client = Client::start();
    let path = "file:///tmp/no-definition.raku";
    client.open(path, "say 1;\n", 1);

    let id = client.request(
        "textDocument/definition",
        serde_json::json!({
            "textDocument": { "uri": path },
            "position": { "line": 0, "character": 5 },
        }),
    );
    let result = client
        .recv_response(id)
        .response_result
        .expect("definition must succeed even with nothing to find");
    assert!(result.is_null(), "{result:#}");

    client.shutdown();
}

#[test]
fn workspace_symbol_searches_files_on_disk() {
    let root = std::env::temp_dir().join(format!("mutsu-lsp-ws-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(root.join("lib")).unwrap();
    std::fs::write(
        root.join("lib/Widget.rakumod"),
        "class Widget {\n    method assemble() { 1 }\n}\n",
    )
    .unwrap();
    std::fs::write(root.join("main.raku"), "sub unrelated() { 1 }\n").unwrap();

    let mut client = Client::start_in_workspace(&root);
    let id = client.request("workspace/symbol", serde_json::json!({ "query": "widget" }));
    let result = client
        .recv_response(id)
        .response_result
        .expect("workspace/symbol must succeed");
    let found = result.as_array().expect("a flat symbol list");
    assert_eq!(
        found.len(),
        1,
        "case-insensitive substring match: {result:#}"
    );
    assert_eq!(found[0]["name"], "Widget");
    assert!(
        found[0]["location"]["uri"]
            .as_str()
            .unwrap()
            .ends_with("lib/Widget.rakumod"),
        "{result:#}"
    );

    // A member of that class is found by its own name, with its container.
    let id = client.request(
        "workspace/symbol",
        serde_json::json!({ "query": "assemble" }),
    );
    let result = client
        .recv_response(id)
        .response_result
        .expect("workspace/symbol must succeed");
    let found = result.as_array().unwrap();
    assert_eq!(found.len(), 1, "{result:#}");
    assert_eq!(found[0]["containerName"], "Widget");

    client.shutdown();
    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn definition_falls_back_to_the_workspace_when_the_open_document_does_not_declare_it() {
    let root = std::env::temp_dir().join(format!("mutsu-lsp-xdef-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).unwrap();
    std::fs::write(
        root.join("Helpers.rakumod"),
        "sub helper() is export { 1 }\n",
    )
    .unwrap();
    let script = root.join("script.raku");
    std::fs::write(&script, "use Helpers;\nsay helper();\n").unwrap();

    let mut client = Client::start_in_workspace(&root);
    let uri = format!("file://{}", script.display());
    client.open(&uri, "use Helpers;\nsay helper();\n", 1);

    let id = client.request(
        "textDocument/definition",
        serde_json::json!({
            "textDocument": { "uri": uri },
            "position": { "line": 1, "character": 6 },
        }),
    );
    let result = client
        .recv_response(id)
        .response_result
        .expect("definition must succeed");
    assert!(
        result["uri"]
            .as_str()
            .unwrap_or("")
            .ends_with("Helpers.rakumod"),
        "{result:#}"
    );
    assert_eq!(result["range"]["start"]["line"], 0);

    client.shutdown();
    let _ = std::fs::remove_dir_all(&root);
}
