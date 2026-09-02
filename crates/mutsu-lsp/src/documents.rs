//! The set of documents the client currently has open.
//!
//! Deliberately a plain `Uri -> text` map. ADR-0065 D3 rejects incremental
//! document sync — a full re-parse costs about 1.3 ms in process, measured by
//! the S0 probe — so there is no diffing, no rope, and no edit application here.
//! Every `didChange` replaces the text outright.

use std::collections::HashMap;

use lsp_types::Uri;

#[derive(Debug, Default)]
pub struct Documents {
    open: HashMap<Uri, Document>,
}

#[derive(Debug, Clone)]
pub struct Document {
    pub text: String,
    /// The client's version for this document. Echoed back on
    /// `publishDiagnostics` so a client can discard a report it has already
    /// superseded.
    pub version: i32,
}

impl Documents {
    /// Record a newly opened document and hand back the stored copy, so the
    /// caller analyses exactly what the store holds rather than a parallel one.
    pub fn open(&mut self, uri: Uri, text: String, version: i32) -> &Document {
        self.open
            .entry(uri)
            .insert_entry(Document { text, version })
            .into_mut()
    }

    /// Apply a full-document change. Returns `None` for a document the server
    /// was never told about, which is a client protocol violation rather than
    /// something to paper over with an empty document.
    pub fn replace(&mut self, uri: &Uri, text: String, version: i32) -> Option<&Document> {
        let doc = self.open.get_mut(uri)?;
        doc.text = text;
        doc.version = version;
        Some(doc)
    }

    pub fn close(&mut self, uri: &Uri) {
        self.open.remove(uri);
    }

    pub fn get(&self, uri: &Uri) -> Option<&Document> {
        self.open.get(uri)
    }

    pub fn len(&self) -> usize {
        self.open.len()
    }

    pub fn is_empty(&self) -> bool {
        self.open.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    fn uri(s: &str) -> Uri {
        Uri::from_str(s).expect("valid uri")
    }

    #[test]
    fn open_then_replace_then_close() {
        let mut docs = Documents::default();
        let u = uri("file:///tmp/a.raku");

        docs.open(u.clone(), "say 1;\n".to_string(), 1);
        assert_eq!(docs.get(&u).map(|d| d.text.as_str()), Some("say 1;\n"));

        let replaced = docs.replace(&u, "say 2;\n".to_string(), 2).cloned();
        assert_eq!(replaced.as_ref().map(|d| d.version), Some(2));
        assert_eq!(docs.get(&u).map(|d| d.text.as_str()), Some("say 2;\n"));

        docs.close(&u);
        assert!(docs.is_empty());
    }

    #[test]
    fn replacing_an_unopened_document_reports_the_miss() {
        let mut docs = Documents::default();
        assert!(
            docs.replace(&uri("file:///tmp/never-opened.raku"), String::new(), 1)
                .is_none()
        );
        assert_eq!(docs.len(), 0);
    }
}
