//! The state machine against the outputs rakudo 2026.09 (MoarVM) gives for
//! the same op sequences.

use super::*;

struct Fixture {
    cfg: DecoderConfig,
    bytes: BufBytes,
    q: TextQueues,
}

impl Fixture {
    fn new(label: &str) -> Fixture {
        Fixture {
            cfg: DecoderConfig {
                codec: Codec::from_label(label).unwrap(),
                translate_nl: false,
                line_separators: DEFAULT_LINE_SEPARATORS
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
            },
            bytes: BufBytes::new(),
            q: TextQueues::default(),
        }
    }

    fn add(&mut self, bytes: &[u8]) {
        self.bytes.extend_from_slice(bytes);
    }

    fn dec(&mut self) -> StreamDecoder<'_> {
        StreamDecoder {
            cfg: &self.cfg,
            bytes: &mut self.bytes,
            q: &mut self.q,
        }
    }
}

#[test]
fn a_line_leaves_the_bytes_behind_it_undecoded() {
    let mut f = Fixture::new("utf8");
    f.add("héllo\nx".as_bytes());
    assert_eq!(
        f.dec().take_line(true, false).unwrap().as_deref(),
        Some("héllo")
    );
    assert_eq!(f.bytes.len(), 1);
    assert_eq!(f.dec().take_line(true, false).unwrap(), None);
    assert_eq!(f.bytes.len(), 0);
    assert!(!f.dec().is_empty());
    assert_eq!(f.dec().take_line(true, true).unwrap().as_deref(), Some("x"));
    assert!(f.dec().is_empty());
    assert_eq!(f.dec().take_line(true, true).unwrap().as_deref(), Some(""));
}

#[test]
fn the_last_grapheme_is_held_for_a_combining_mark() {
    let mut f = Fixture::new("utf8");
    f.add(b"a");
    assert_eq!(f.dec().take_available_chars().unwrap(), "");
    assert!(!f.dec().is_empty());
    assert_eq!(f.bytes.len(), 0);
    f.add(&[0xCC, 0x81, 0x62]);
    assert_eq!(f.dec().take_available_chars().unwrap(), "\u{E1}");
    assert_eq!(f.dec().take_all_chars().unwrap(), "b");
    assert_eq!(f.dec().take_all_chars().unwrap(), "");
}

#[test]
fn a_split_multibyte_character_waits_for_its_tail() {
    let mut f = Fixture::new("utf8");
    f.add(&[0xC3]);
    assert_eq!(f.dec().take_available_chars().unwrap(), "");
    assert_eq!(f.bytes.len(), 1);
    f.add(&[0xA9, 0x41, 0x42, 0x43]);
    assert_eq!(f.dec().take_chars(2, false).unwrap().as_deref(), Some("éA"));
    assert_eq!(f.dec().take_chars(5, false).unwrap(), None);
    assert_eq!(f.dec().take_chars(1, false).unwrap().as_deref(), Some("B"));
    // `C` is still pending: a combining mark could follow it.
    assert_eq!(f.dec().take_chars(1, false).unwrap(), None);
    assert_eq!(f.dec().take_chars(5, true).unwrap().as_deref(), Some("C"));
}

#[test]
fn incomplete_bytes_at_the_end_are_an_error_and_stay_buffered() {
    let mut f = Fixture::new("utf8");
    f.add(&[0x61, 0xC3]);
    let err = f.dec().take_all_chars().unwrap_err();
    assert_eq!(
        err.message,
        "Incomplete character near bytes c3 at the end of a stream"
    );
    assert_eq!(f.bytes.len(), 1);
}

#[test]
fn custom_separators_match_the_earliest_then_longest() {
    let mut f = Fixture::new("utf8");
    f.cfg.line_separators = vec!["ab".to_string(), "\n".to_string()];
    f.add(b"xabyy\nzz");
    assert_eq!(
        f.dec().take_line(false, false).unwrap().as_deref(),
        Some("xab")
    );
    assert_eq!(
        f.dec().take_line(true, false).unwrap().as_deref(),
        Some("yy")
    );
    assert_eq!(
        f.dec().take_line(false, true).unwrap().as_deref(),
        Some("zz")
    );
}

#[test]
fn a_trailing_cr_waits_for_a_possible_lf() {
    let mut f = Fixture::new("utf8");
    f.add(b"a\r");
    assert_eq!(f.dec().take_line(true, false).unwrap(), None);
    f.add(b"\nb");
    assert_eq!(
        f.dec().take_line(true, false).unwrap().as_deref(),
        Some("a")
    );
    assert_eq!(f.dec().take_line(true, true).unwrap().as_deref(), Some("b"));
}

#[test]
fn the_lf_separator_does_not_split_a_crlf_grapheme() {
    let mut f = Fixture::new("latin1");
    f.cfg.line_separators = vec!["\n".to_string()];
    f.add(b"a\r\nb\n");
    assert_eq!(
        f.dec().take_line(false, false).unwrap().as_deref(),
        Some("a\r\nb\n")
    );
}

#[test]
fn single_byte_codecs_hold_only_a_carriage_return() {
    for label in ["iso-8859-1", "windows-1252", "ascii"] {
        let mut f = Fixture::new(label);
        f.add(b"ab\r");
        assert_eq!(f.dec().take_available_chars().unwrap(), "ab", "{label}");
        f.add(b"\nc");
        assert_eq!(f.dec().take_available_chars().unwrap(), "\r\nc", "{label}");
    }
    let mut f = Fixture::new("windows-1252");
    f.add(&[0x80, 0x41]);
    assert_eq!(f.dec().take_all_chars().unwrap(), "€A");
}

#[test]
fn translate_nl_hands_out_crlf_as_lf() {
    let mut f = Fixture::new("utf8");
    f.cfg.translate_nl = true;
    f.add(b"a\r\nb\rc\r");
    assert_eq!(f.dec().take_all_chars().unwrap(), "a\nb\rc\r");
}

#[test]
fn take_bytes_reads_only_the_undecoded_queue() {
    let mut f = Fixture::new("utf8");
    f.add(&[1, 2, 3, 4]);
    assert_eq!(f.dec().take_bytes(3), Some(vec![1, 2, 3]));
    assert_eq!(f.dec().take_bytes(3), None);
    assert_eq!(f.bytes.len(), 1);
}

#[test]
fn malformed_utf8_is_an_error() {
    let mut f = Fixture::new("utf8");
    f.add(&[0x61, 0xFF, 0x62]);
    let err = f.dec().take_all_chars().unwrap_err();
    assert_eq!(err.message, "Malformed UTF-8 near byte ff");
    let mut f = Fixture::new("ascii");
    f.add(&[0x61, 0xFF]);
    let err = f.dec().take_all_chars().unwrap_err();
    assert_eq!(
        err.message,
        "Will not decode invalid ASCII (code point (255) > 127 found)"
    );
}

#[test]
fn a_line_trickling_in_is_found_once_complete() {
    let mut f = Fixture::new("utf8");
    f.cfg.line_separators = vec!["\r\n".to_string()];
    for chunk in [&b"abc"[..], b"def\r", b"\nnext"] {
        f.add(chunk);
        let got = f.dec().take_line(true, false).unwrap();
        if chunk.starts_with(b"\n") {
            assert_eq!(got.as_deref(), Some("abcdef"));
        } else {
            assert_eq!(got, None);
        }
    }
}
