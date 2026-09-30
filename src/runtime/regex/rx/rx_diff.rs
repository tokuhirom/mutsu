//! ADR-0135 D6: the comparison `MUTSU_RX_DIFF=1` applies to every compiled
//! match against the walk's answer for the same start position. Deleted with
//! the walk (D7).

use crate::runtime::regex_types::{CapNode, PosSlot, RegexCaptures};

fn node_span(node: &CapNode) -> String {
    let kids = node
        .children
        .as_ref()
        .map_or(0, |c| c.named.len() + c.positional.len());
    format!(
        "{}..{} kids={kids} action={:?}",
        node.from, node.to, node.action_name
    )
}

fn slot_desc(slot: &PosSlot) -> String {
    format!(
        "{}..{} subcap={:?} quantified={:?} nil={}",
        slot.from,
        slot.to,
        slot.subcap.as_deref().map(node_span),
        slot.quantified
            .as_ref()
            .map(|q| q.iter().map(|(f, t, _)| (*f, *t)).collect::<Vec<_>>()),
        slot.nil
    )
}

fn caps_desc(caps: &RegexCaptures) -> String {
    let mut named: Vec<String> = caps
        .named
        .iter()
        .map(|(k, v)| {
            format!(
                "{}{}=[{}]",
                k.resolve(),
                if v.quantified { "(q)" } else { "" },
                v.nodes
                    .iter()
                    .map(|n| node_span(n))
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        })
        .collect();
    named.sort();
    format!(
        "positional=[{}] named={{{}}} capture_start={:?} capture_end={:?} match_from={}",
        caps.positional
            .iter()
            .map(slot_desc)
            .collect::<Vec<_>>()
            .join("; "),
        named.join(" "),
        caps.capture_start,
        caps.capture_end,
        caps.match_from
    )
}

/// `Ok` when the two engines reported the same match (or both none).
pub(super) fn same_match(
    compiled: &Option<(usize, RegexCaptures)>,
    walked: &Option<(usize, RegexCaptures)>,
) -> Result<(), String> {
    let render = |m: &Option<(usize, RegexCaptures)>| match m {
        None => "no match".to_string(),
        Some((end, caps)) => format!("end {end}, {}", caps_desc(caps)),
    };
    let (c, w) = (render(compiled), render(walked));
    if c == w {
        Ok(())
    } else {
        Err(format!("compiled: {c}\n  walked: {w}"))
    }
}
