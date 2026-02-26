use std::sync::OnceLock;

static TRACE_CONFIG: OnceLock<TraceConfig> = OnceLock::new();

struct TraceConfig {
    all: bool,
    phases: Vec<String>,
}

fn config() -> &'static TraceConfig {
    TRACE_CONFIG.get_or_init(|| {
        #[cfg(target_arch = "wasm32")]
        let val = String::new();
        #[cfg(not(target_arch = "wasm32"))]
        let val = std::env::var("MUTSU_TRACE").unwrap_or_default();
        if val.is_empty() || val == "0" {
            TraceConfig {
                all: false,
                phases: vec![],
            }
        } else if val == "1" {
            TraceConfig {
                all: true,
                phases: vec![],
            }
        } else {
            TraceConfig {
                all: false,
                phases: val.split(',').map(|s| s.to_string()).collect(),
            }
        }
    })
}

pub fn is_enabled(phase: &str) -> bool {
    let c = config();
    c.all || c.phases.iter().any(|p| p == phase)
}

macro_rules! trace_log {
    ($phase:expr, $($arg:tt)*) => {
        if $crate::trace::is_enabled($phase) {
            eprintln!("[TRACE:{}] {}", $phase, format!($($arg)*));
        }
    };
}
pub(crate) use trace_log;
