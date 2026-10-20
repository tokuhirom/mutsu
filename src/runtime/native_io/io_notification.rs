//! `IO::Notification.watch-path` / `IO::Path.watch` and the
//! `IO::Notification::Change` events they emit (#9586).
//!
//! # Mechanism
//!
//! A registered GC-helper thread polls a per-entry snapshot of the watched
//! path and diffs consecutive snapshots, rather than subscribing to an OS
//! notification API (inotify / kqueue / FSEvents / ReadDirectoryChangesW).
//! Polling is the one mechanism that behaves the same on every release target
//! without a per-platform dependency, cannot miss an event because a kernel
//! queue overflowed, and needs no file descriptor that must be released when a
//! tap closes. Its cost is `O(entries)` `stat` calls per poll for a directory,
//! which is what an interactive `watch-path` (a project directory, a log file)
//! can afford.
//!
//! The event kinds follow rakudo's libuv mapping: an entry appearing or
//! disappearing is `FileRenamed`, a changed entry (size, mtime, mode, inode)
//! is `FileChanged`. For a watched directory `.path` is the entry's path, the
//! watched path joined with the entry name (`tmp/w` + `x` = `tmp/w/x`); for a
//! watched file it is the watched path itself.
//!
//! # Lifetime
//!
//! Like rakudo's, the watch effectively begins at the first tap: until a tap
//! subscribes, each poll only refreshes the baseline snapshot, so what
//! happened before the tap is not reported (to within one poll interval).
//!
//! The watcher retires as soon as its supply can never deliver again: a tap
//! closed (`Tap.close` flags the channel), or every tap it ever had was
//! dropped. It checks this on every poll, so a quiet directory does not pin a
//! thread after its last tap is gone.

mod watch_state;

use super::*;
use crate::runtime::native_methods::SupplyEvent;
use crate::runtime::nqp_ops_async::QueueSink;
use crate::value::{AttrMap, EnumValue};
use std::time::Duration;
use watch_state::{ChangeKind, WatchState};

/// The enum type `IO::Notification::Change.event` holds.
const FILE_CHANGE_EVENT: &str = "FileChangeEvent";
/// The event class the watcher emits.
const CHANGE_CLASS: &str = "IO::Notification::Change";
/// How often the watcher re-reads its snapshot.
const POLL_INTERVAL: Duration = Duration::from_millis(25);

/// An `IO::Notification::Change` carrying `path` and `event`.
fn change_value(path: String, kind: ChangeKind) -> Value {
    let mut attrs = HashMap::new();
    attrs.insert("path".to_string(), Value::str(path));
    attrs.insert("event".to_string(), file_change_event(kind));
    Value::make_instance(Symbol::intern(CHANGE_CLASS), attrs)
}

fn file_change_event(kind: ChangeKind) -> Value {
    let (index, value) = match kind {
        ChangeKind::Changed => (0, 1),
        ChangeKind::Renamed => (1, 2),
    };
    Value::enum_parts(
        Symbol::intern(FILE_CHANGE_EVENT),
        Symbol::intern(kind.key()),
        EnumValue::Int(value),
        index,
    )
}

/// libuv's message for a watch on a missing path, as rakudo reports it.
const MISSING_PATH_MESSAGE: &str = "no such file or directory";

/// The `X::AdHoc` a watch on a missing path quits with.
fn missing_path_exception() -> Value {
    let mut attrs = HashMap::new();
    attrs.insert(
        "message".to_string(),
        Value::str_from(MISSING_PATH_MESSAGE),
    );
    Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
}

impl Interpreter {
    /// The `FileChangeEvent` enum's variants (`enum FileChangeEvent
    /// (:FileChanged(1), :FileRenamed(2))`). Shared by
    /// [`Self::init_file_change_event_enum`] and
    /// [`Self::seed_builtin_enum_types`].
    pub(in crate::runtime) fn file_change_event_enum_variants() -> Vec<(String, EnumValue)> {
        vec![
            ("FileChanged".to_string(), EnumValue::Int(1)),
            ("FileRenamed".to_string(), EnumValue::Int(2)),
        ]
    }

    pub(in crate::runtime) fn init_file_change_event_enum(base: &mut HashMap<Symbol, Value>) {
        base.insert(
            Symbol::intern(FILE_CHANGE_EVENT),
            Value::str_from(FILE_CHANGE_EVENT),
        );
        for (index, (key, val)) in Self::file_change_event_enum_variants()
            .into_iter()
            .enumerate()
        {
            let enum_val = Value::enum_parts(
                Symbol::intern(FILE_CHANGE_EVENT),
                Symbol::intern(&key),
                val,
                index,
            );
            base.insert(
                crate::qualified::qualified(
                    Symbol::intern(FILE_CHANGE_EVENT),
                    Symbol::intern(&key),
                ),
                enum_val.clone(),
            );
            base.insert(Symbol::intern(&key), enum_val);
        }
    }

    /// The built-in `IO::Notification` (a placeholder for its `watch-path`
    /// type-object method) and `IO::Notification::Change` classes. The
    /// latter's `path` / `event` are ordinary public attributes, so their
    /// accessors are the generated ones and `.new(:path, :event)` / `.raku`
    /// work like any class's.
    pub(in crate::runtime) fn io_notification_class_defs() -> [(String, ClassDef); 2] {
        let public_attr = |name: &str| ClassAttributeDef {
            name: name.to_string(),
            is_public: true,
            default: None,
            captured_env: None,
            captured_unit: None,
            declaring_package: None,
            is_rw: false,
            is_required: None,
            sigil: '$',
            type_constraint: None,
            where_constraint: None,
            declared_shape: None,
            source_line: None,
            source_file: None,
            default_is_seed: false,
            default_is_trait: false,
        };
        let class_def =
            |name: &str, attributes: Vec<ClassAttributeDef>, methods: &[&str]| ClassDef {
                parents: Vec::new(),
                attributes,
                native_methods: methods.iter().map(|m| m.to_string()).collect(),
                mro: crate::runtime::sym_mro(&[name]),
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                embedded_attributes: HashSet::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: ValueMap::default(),
            };
        [
            (
                "IO::Notification".to_string(),
                class_def("IO::Notification", Vec::new(), &["watch-path"]),
            ),
            (
                CHANGE_CLASS.to_string(),
                class_def(
                    CHANGE_CLASS,
                    vec![public_attr("path"), public_attr("event")],
                    &["IO", "gist"],
                ),
            ),
        ]
    }

    /// The VM's native instance lane for `IO::Notification::Change`'s methods
    /// (`IO::Path.watch` is a row of the method table).
    pub(crate) fn try_io_notification_instance_method(
        &self,
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        let class = class_name.resolve();
        let attributes = attributes.as_map();
        self.try_io_notification_change_method(&class, &attributes, method)
    }

    /// `IO::Path.watch`: the method form of `IO::Notification.watch-path`,
    /// reporting paths under the receiver's absolute path (as rakudo does).
    /// Shared by the `watch` row and `IO::Notification.watch-path`.
    // Cost: O(1) here; the watcher thread pays O(n) per poll, n = entries of
    // the watched directory (1 for a file).
    pub(crate) fn io_path_watch(&self, attributes: &AttrMap) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let watched = self.resolve_io_path_buf(attributes, &p);
        let display = match self.io_path_absolute(attributes, &[]) {
            Ok(abs) => abs.to_string_value(),
            Err(_) => Self::stringify_path(&watched),
        };
        Self::watch_path_supply(watched, display)
    }

    /// Type-object methods of `IO::Notification`: `watch-path(Str() $path)`.
    /// Shared by the VM's native class-method lane and the interpreter's
    /// type-object dispatch.
    // Cost: O(1) here; the watcher thread pays O(n) per poll, n = entries of
    // the watched directory (1 for a file).
    pub(crate) fn try_io_notification_class_method(
        &self,
        class_name: Symbol,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if class_name != "IO::Notification" || method != "watch-path" {
            return None;
        }
        let Some(path) = args
            .iter()
            .find(|a| !matches!(a.view(), ValueView::Pair(..)))
        else {
            return Some(Err(RuntimeError::new(
                "Too few positionals passed; expected 2 arguments but got 1",
            )));
        };
        // Rakudo turns an `IO::Path` argument into its absolute path (so the
        // events are reported under it, exactly as `IO::Path.watch` does);
        // anything else is `Str()`-coerced and reported as written.
        if let ValueView::Instance { attributes, .. } = path.view()
            && Self::is_io_path_value(path)
        {
            return Some(self.io_path_watch(&attributes.as_map()));
        }
        let p = path.to_string_value();
        let watched = self.resolve_path(&p);
        Some(Self::watch_path_supply(watched, p))
    }

    fn is_io_path_value(value: &Value) -> bool {
        matches!(value.view(), ValueView::Instance { class_name, .. }
            if class_name == "IO::Path" || class_name.as_str().starts_with("IO::Path::"))
    }

    /// Methods of an `IO::Notification::Change` beyond its generated `path` /
    /// `event` accessors: `.IO` (the path as an `IO::Path`) and `.gist`
    /// (`path: event`). Shared by the VM's native instance lane and the
    /// interpreter's native-class dispatch.
    // Cost: O(len(path)).
    pub(crate) fn try_io_notification_change_method(
        &self,
        class_name: &str,
        attributes: &AttrMap,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        if class_name != CHANGE_CLASS {
            return None;
        }
        let path = || {
            attributes
                .get("path")
                .map(|v| v.to_string_value())
                .unwrap_or_default()
        };
        match method {
            "IO" => Some(Ok(self.make_io_path_instance(&path()))),
            "gist" => {
                let event = attributes
                    .get("event")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Some(Ok(Value::str(format!("{}: {}", path(), event))))
            }
            _ => None,
        }
    }

    /// Start a watcher over `watched` and return the live `Supply` its
    /// changes are emitted on, reported under `display`.
    fn watch_path_supply(watched: PathBuf, display: String) -> Result<Value, RuntimeError> {
        use crate::runtime::native_methods as nm;
        let supply_id = nm::next_supply_id();
        let (tx, rx) = nm::supply_channel::supply_event_channel();
        if let Ok(mut map) = nm::supply_channel_map_pub().lock() {
            map.insert(supply_id, rx);
        }
        if let Err(e) = spawn_watcher(watched, display, WatchSink::Supply(tx)) {
            nm::discard_supply_channel(supply_id);
            return Err(e);
        }
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("supply_id".to_string(), Value::int(supply_id as i64));
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// `nqp::watchfile`'s watcher (ADR-11502 §2.4.2): the same poller as
    /// `watch-path`, pushing `[schedulee, name, renamed, error]` onto the
    /// op's queue until its task is cancelled.
    // Cost: O(1) here; the watcher thread pays O(n) per poll, n = entries of
    // the watched directory (1 for a file).
    pub(crate) fn watch_path_queue(&self, path: &str, sink: QueueSink) -> Result<(), RuntimeError> {
        let watched = self.resolve_path(path);
        spawn_watcher(watched, String::new(), WatchSink::Queue(sink))
    }
}

/// Where a watcher reports: the `Supply` of `watch-path`, or the queue of an
/// `nqp::watchfile` task.
enum WatchSink {
    Supply(crate::runtime::native_methods::supply_channel::SupplySender),
    Queue(QueueSink),
}

impl WatchSink {
    /// Whether nothing can receive any more: the watcher thread ends.
    // Cost: O(1).
    fn is_retired(&self) -> bool {
        match self {
            WatchSink::Supply(tx) => tx.is_retired(),
            WatchSink::Queue(sink) => sink.is_cancelled(),
        }
    }

    /// Whether changes are reported yet. A Supply starts at its first tap,
    /// as Rakudo's does; an `nqp::watchfile` task at once.
    // Cost: O(1).
    fn is_watching(&self) -> bool {
        match self {
            WatchSink::Supply(tx) => tx.ever_subscribed(),
            WatchSink::Queue(_) => true,
        }
    }

    /// Report the watched path missing; the watcher then ends.
    // Cost: O(1).
    fn missing(&self) {
        match self {
            WatchSink::Supply(tx) => {
                let _ = tx.send(SupplyEvent::Quit(missing_path_exception()));
            }
            // MoarVM reports libuv's error as the fourth element, with type
            // objects for the name and the event kind.
            WatchSink::Queue(sink) => sink.push(vec![
                Value::package(Symbol::intern("BOOTStr")),
                Value::package(Symbol::intern("BOOTInt")),
                Interpreter::boot_str(MISSING_PATH_MESSAGE),
            ]),
        }
    }

    /// Report one change. `path` is the watched path as displayed, joined
    /// with the entry name for a directory; `fallback` is the name the queue
    /// form reports for the watched path itself (its basename, as libuv does).
    // Cost: O(len(path)).
    fn report(&self, path: String, kind: ChangeKind, fallback: &str) -> bool {
        match self {
            WatchSink::Supply(tx) => tx.send(SupplyEvent::Emit(change_value(path, kind))).is_ok(),
            WatchSink::Queue(sink) => {
                let name = if path.is_empty() { fallback.to_string() } else { path };
                let renamed = matches!(kind, ChangeKind::Renamed) as i64;
                sink.push(vec![
                    Interpreter::boot_str(&name),
                    Interpreter::boot_int(renamed),
                    Value::package(Symbol::intern("BOOTStr")),
                ]);
                true
            }
        }
    }
}

/// Poll `watched` on a registered helper thread and report its changes to
/// `sink`, under `display` (empty for the queue form, which reports entry
/// names).
fn spawn_watcher(watched: PathBuf, display: String, sink: WatchSink) -> Result<(), RuntimeError> {
    let basename = watched
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_default();
    // Registered spawn (emits `Value`s into a channel); the poll sleep is a
    // quiescent safe region — see `try_spawn_gc_helper_thread`. A refused
    // thread is a catchable X::AdHoc (#9401).
    crate::runtime::builtins_system::try_spawn_gc_helper_thread("io-notification", move || {
        let mut last = WatchState::snapshot(&watched);
        if last == WatchState::File(None) {
            sink.missing();
            return;
        }
        loop {
            crate::gc::block_quiescent(|| std::thread::sleep(POLL_INTERVAL));
            if sink.is_retired() {
                return;
            }
            let next = WatchState::snapshot(&watched);
            // Until the sink is watching, only the baseline moves.
            if !sink.is_watching() {
                last = next;
                continue;
            }
            for (path, kind) in last.diff(&next, &display) {
                if !sink.report(path, kind, &basename) {
                    return;
                }
            }
            last = next;
        }
    })
    .map(|_| ())
    .map_err(crate::runtime::builtins_system::refused_thread_error)
}
