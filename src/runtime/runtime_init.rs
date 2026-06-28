use super::*;

impl Interpreter {
    /// Take any pending regex security error from the thread-local store.
    pub(crate) fn take_pending_regex_error() -> Option<RuntimeError> {
        // Delegate to the regex_parse module's thread-local error store
        regex_parse::PENDING_REGEX_ERROR.with(|e| e.borrow_mut().take())
    }

    pub fn new() -> Self {
        let mut env = HashMap::new();
        env.insert("*PID".to_string(), Value::Int(current_process_id()));
        env.insert("*TZ".to_string(), Value::Int(local_timezone_offset_secs()));
        env.insert("@*ARGS".to_string(), Value::real_array(Vec::new()));
        env.insert("*INIT-INSTANT".to_string(), Value::make_instant_now());
        // Populate %*ENV with all OS environment variables so that
        // %*ENV.keys, %*ENV.elems, and copying %*ENV work correctly.
        {
            let mut env_hash = HashMap::new();
            #[cfg(not(target_family = "wasm"))]
            for (key, value) in std::env::vars() {
                env_hash.insert(key, builtins_collection::builtin_val(&[Value::str(value)]));
            }
            env.insert("%*ENV".to_string(), Value::Hash(Value::hash_arc(env_hash)));
        }
        env.insert(
            "*SCHEDULER".to_string(),
            Value::make_instance(Symbol::intern("ThreadPoolScheduler"), HashMap::new()),
        );
        let mut classes = HashMap::new();
        classes.insert(
            "Mu".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Mu".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Any".to_string(),
            ClassDef {
                parents: vec!["Mu".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Any".to_string(), "Mu".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "IterationBuffer".to_string(),
            ClassDef {
                parents: vec!["Any".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "IterationBuffer".to_string(),
                    "Any".to_string(),
                    "Mu".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Promise".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["keep", "result", "status", "then"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Promise".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Promise::Vow".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["keep", "break"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Promise::Vow".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Channel".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["send", "receive", "close", "closed"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Channel".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Collation".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Collation".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Thread".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["finish", "id"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Thread".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Supply".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "emit",
                    "tap",
                    "act",
                    "encode",
                    "decode",
                    "repeated",
                    "do",
                    "reverse",
                    "split",
                    "interval",
                    "tail",
                    "delayed",
                    "min",
                    "minmax",
                    "collate",
                    "lines",
                    "merge",
                    "unique",
                    "on-close",
                    "classify",
                    "categorize",
                    "Channel",
                    "Supply",
                    "Promise",
                    "schedule-on",
                    "native-descriptor",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Supply".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "utf8".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["utf8".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "utf16".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["utf16".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Blob".to_string(),
            ClassDef {
                parents: vec!["Any".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Blob".to_string(), "Any".to_string(), "Mu".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Buf".to_string(),
            ClassDef {
                parents: vec!["Blob".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "Buf".to_string(),
                    "Blob".to_string(),
                    "Any".to_string(),
                    "Mu".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Supplier".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "emit",
                    "done",
                    "quit",
                    "Supply",
                    "__mutsu_register_close_phaser",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Supplier".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Supplier::Preserving".to_string(),
            ClassDef {
                parents: vec!["Supplier".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Supplier::Preserving".to_string(), "Supplier".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Proc::Async".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "start",
                    "command",
                    "started",
                    "w",
                    "pid",
                    "stdout",
                    "stderr",
                    "Supply",
                    "bind-stdin",
                    "bind-stdout",
                    "bind-stderr",
                    "kill",
                    "write",
                    "close-stdin",
                    "ready",
                    "print",
                    "say",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Proc::Async".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Proc".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "exitcode", "signal", "command", "pid", "err", "out", "in", "Numeric", "Int",
                    "Bool", "Str", "gist", "spawn", "shell", "run",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Proc".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Tap".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cancel", "close", "socket-port", "socket-host"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Tap".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Scheduler".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cue"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Scheduler".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "ThreadPoolScheduler".to_string(),
            ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cue", "uncaught_handler"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["ThreadPoolScheduler".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "CurrentThreadScheduler".to_string(),
            ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cue", "uncaught_handler"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["CurrentThreadScheduler".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "FakeScheduler".to_string(),
            ClassDef {
                parents: vec!["Scheduler".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cue", "progress-by", "time"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["FakeScheduler".to_string(), "Scheduler".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Cancellation".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["cancel"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Cancellation".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Lock".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["protect", "lock", "unlock", "condition"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Lock".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Lock::Async".to_string(),
            ClassDef {
                parents: vec!["Lock".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Lock::Async".to_string(), "Lock".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Lock::ConditionVariable".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["wait", "signal", "signal_all"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Lock::ConditionVariable".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Semaphore".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["acquire", "try_acquire", "release"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Semaphore".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "IO::Path".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "Str",
                    "gist",
                    "raku",
                    "perl",
                    "IO",
                    "Numeric",
                    "Real",
                    "Int",
                    "Rat",
                    "Num",
                    "FatRat",
                    "basename",
                    "dirname",
                    "cleanup",
                    "parts",
                    "parent",
                    "sibling",
                    "child",
                    "add",
                    "extension",
                    "absolute",
                    "relative",
                    "resolve",
                    "volume",
                    "is-absolute",
                    "is-relative",
                    "e",
                    "f",
                    "d",
                    "l",
                    "r",
                    "w",
                    "x",
                    "rw",
                    "rwx",
                    "mode",
                    "s",
                    "z",
                    "created",
                    "modified",
                    "accessed",
                    "changed",
                    "lines",
                    "words",
                    "slurp",
                    "open",
                    "copy",
                    "rename",
                    "move",
                    "chmod",
                    "mkdir",
                    "rmdir",
                    "dir",
                    "spurt",
                    "unlink",
                    "symlink",
                    "starts-with",
                    "watch",
                    "succ",
                    "pred",
                    "CWD",
                    "SPEC",
                    "link",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["IO::Path".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "IO::Handle".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "path",
                    "IO",
                    "Str",
                    "gist",
                    "DESTROY",
                    "close",
                    "get",
                    "getc",
                    "readchars",
                    "lines",
                    "words",
                    "read",
                    "write",
                    "print",
                    "say",
                    "put",
                    "flush",
                    "seek",
                    "tell",
                    "eof",
                    "encoding",
                    "opened",
                    "slurp",
                    "out-buffer",
                    "Supply",
                    "open",
                    "nl-out",
                    "nl-in",
                    "print-nl",
                    "native-descriptor",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["IO::Handle".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Backtrace".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Backtrace".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "CompUnit::Repository::FileSystem".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["CompUnit::Repository::FileSystem".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "IO::Pipe".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["slurp", "Str", "gist", "print", "close"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["IO::Pipe".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "IO::Socket::INET".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "close",
                    "getpeername",
                    "accept",
                    "localport",
                    "print",
                    "say",
                    "put",
                    "write",
                    "recv",
                    "read",
                    "get",
                    "lines",
                    "nl-in",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["IO::Socket::INET".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "IO::Socket::Async".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "close",
                    "write",
                    "print",
                    "Supply",
                    "socket-port",
                    "peer-port",
                    "socket-host",
                    "peer-host",
                    "print-to",
                    "write-to",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["IO::Socket::Async".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "IO::Socket::Async::Listener".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["tap", "act"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["IO::Socket::Async::Listener".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Distro".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "name",
                    "auth",
                    "desc",
                    "release",
                    "path-sep",
                    "is-win",
                    "version",
                    "signature",
                    "gist",
                    "Str",
                    "raku",
                    "perl",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Distro".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Perl".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "DISTROnames",
                    "KERNELnames",
                    "compiler",
                    "backend",
                    "name",
                    "auth",
                    "version",
                    "signature",
                    "desc",
                    "gist",
                    "raku",
                    "Str",
                    "release",
                    "codename",
                    "id",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Perl".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Kernel".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "name",
                    "auth",
                    "version",
                    "signature",
                    "desc",
                    "release",
                    "hardware",
                    "arch",
                    "bits",
                    "hostname",
                    "signals",
                    "signal",
                    "cpu-cores",
                    "endian",
                    "gist",
                    "raku",
                    "Str",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Kernel".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "VM".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "name",
                    "auth",
                    "version",
                    "precomp-ext",
                    "precomp-target",
                    "request-garbage-collection",
                    "gist",
                    "Str",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["VM".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Compiler".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "name",
                    "auth",
                    "version",
                    "signature",
                    "desc",
                    "gist",
                    "raku",
                    "Str",
                    "release",
                    "codename",
                    "id",
                    "backend",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Compiler".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Encoding::Builtin".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["name", "alternative-names", "encoder", "decoder"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                mro: vec!["Encoding::Builtin".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Encoding::Encoder".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["encode-chars"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Encoding::Encoder".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Encoding::Decoder".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: [
                    "decode-chars",
                    "add-bytes",
                    "consume-all-chars",
                    "consume-available-chars",
                    "bytes-available",
                    "is-empty",
                    "set-line-separators",
                ]
                .iter()
                .map(|s| s.to_string())
                .collect(),
                mro: vec!["Encoding::Decoder".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Encoding::Registry".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: ["find", "register"].iter().map(|s| s.to_string()).collect(),
                mro: vec!["Encoding::Registry".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Block".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Block::Code".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Block::Code".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::FormattingCode".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::FormattingCode".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Block::Comment".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Block::Comment".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Block::Para".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Block::Para".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Block::Named".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Block::Named".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Heading".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Heading".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Block::Table".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Block::Table".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Config".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Config".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Pod::Item".to_string(),
            ClassDef {
                parents: vec!["Pod::Block".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Pod::Item".to_string(), "Pod::Block".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "Exception".to_string(),
            ClassDef {
                parents: vec![],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::AdHoc".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::AdHoc".to_string(), "Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::TypeCheck".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::TypeCheck".to_string(), "Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::TypeCheck::Binding".to_string(),
            ClassDef {
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "X::TypeCheck::Binding".to_string(),
                    "X::TypeCheck".to_string(),
                    "Exception".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::TypeCheck::Binding::Parameter".to_string(),
            ClassDef {
                parents: vec!["X::TypeCheck::Binding".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "X::TypeCheck::Binding::Parameter".to_string(),
                    "X::TypeCheck::Binding".to_string(),
                    "X::TypeCheck".to_string(),
                    "Exception".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::Parameter".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::Parameter".to_string(), "Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::Parameter::InvalidConcreteness".to_string(),
            ClassDef {
                parents: vec!["X::Parameter".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "X::Parameter::InvalidConcreteness".to_string(),
                    "X::Parameter".to_string(),
                    "Exception".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::Supply::Combinator".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::Supply::Combinator".to_string(), "Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::TypeCheck::Argument".to_string(),
            ClassDef {
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "X::TypeCheck::Argument".to_string(),
                    "X::TypeCheck".to_string(),
                    "Exception".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::TypeCheck::Assignment".to_string(),
            ClassDef {
                parents: vec!["X::TypeCheck".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec![
                    "X::TypeCheck::Assignment".to_string(),
                    "X::TypeCheck".to_string(),
                    "Exception".to_string(),
                ],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::Numeric::Real".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::Numeric::Real".to_string(), "Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::TypeCheck::Return".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::TypeCheck::Return".to_string(), "Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );
        classes.insert(
            "X::Coerce::Impossible".to_string(),
            ClassDef {
                parents: vec!["Exception".to_string()],
                attributes: Vec::new(),
                methods: HashMap::new(),
                native_methods: HashSet::new(),
                mro: vec!["X::Coerce::Impossible".to_string(), "Exception".to_string()],
                attribute_types: HashMap::new(),
                attribute_smileys: HashMap::new(),
                attribute_built: HashMap::new(),
                wildcard_handles: Vec::new(),
                alias_attributes: HashSet::new(),
                class_level_attrs: HashMap::new(),
            },
        );

        // Register additional X:: exception classes using a helper closure
        // to reduce boilerplate.
        let mut register_x = |name: &str, parent: &str| {
            let mut mro = vec![name.to_string()];
            // Walk up through existing classes to build full MRO
            let mut cur = parent.to_string();
            loop {
                mro.push(cur.clone());
                if let Some(cls) = classes.get(&cur)
                    && let Some(p) = cls.parents.first()
                    && p != &cur
                {
                    cur = p.clone();
                    continue;
                }
                break;
            }
            if !mro.contains(&"Exception".to_string()) {
                mro.push("Exception".to_string());
            }
            classes.insert(
                name.to_string(),
                ClassDef {
                    parents: vec![parent.to_string()],
                    attributes: Vec::new(),
                    methods: HashMap::new(),
                    native_methods: HashSet::new(),
                    mro,
                    attribute_types: HashMap::new(),
                    attribute_smileys: HashMap::new(),
                    attribute_built: HashMap::new(),
                    wildcard_handles: Vec::new(),
                    alias_attributes: HashSet::new(),
                    class_level_attrs: HashMap::new(),
                },
            );
        };

        // X::Comp hierarchy (compile-time errors)
        register_x("X::Comp", "Exception");
        register_x("X::Comp::Group", "X::Comp");
        register_x("X::Comp::AdHoc", "X::Comp");
        register_x("X::Comp::NYI", "X::Comp");
        register_x("X::Composition::NotComposable", "Exception");
        register_x("X::Value", "Exception");
        register_x("X::Value::Dynamic", "X::Value");

        // X::Syntax hierarchy (syntax errors, subtypes of X::Comp)
        register_x("X::Syntax", "X::Comp");
        register_x("X::Syntax::Confused", "X::Syntax");
        register_x("X::Syntax::BlockGobbled", "X::Syntax");
        register_x("X::Syntax::Extension::Null", "X::Syntax");
        register_x("X::Syntax::Missing", "X::Syntax");
        register_x("X::Syntax::VirtualCall", "X::Syntax");
        register_x("X::Syntax::NegatedPair", "X::Syntax");
        register_x("X::Syntax::Malformed", "X::Syntax");
        register_x("X::Syntax::Variable::Numeric", "X::Syntax");
        register_x("X::Syntax::Variable::Initializer", "X::Syntax");
        register_x("X::Syntax::Variable::IndirectDeclaration", "X::Syntax");
        register_x("X::Syntax::Variable::ConflictingTypes", "X::Syntax");
        register_x("X::Syntax::Number::LiteralType", "X::Syntax");
        register_x("X::Syntax::Regex::Adverb", "X::Syntax");
        register_x("X::Backslash::UnrecognizedSequence", "X::Backslash");
        register_x("X::Syntax::Regex::SolitaryQuantifier", "X::Syntax");
        register_x("X::Syntax::Regex::NullRegex", "X::Syntax");
        register_x("X::Syntax::Regex::NonQuantifiable", "X::Syntax");
        register_x("X::Syntax::Regex::QuantifierValue", "X::Syntax");
        register_x("X::Syntax::Term::MissingInitializer", "X::Syntax");
        register_x("X::Syntax::WithoutElse", "X::Syntax");
        register_x("X::Syntax::UnlessElse", "X::Syntax");
        register_x("X::Syntax::Reserved", "X::Syntax");
        register_x("X::Syntax::KeywordAsFunction", "X::Syntax");
        register_x("X::Syntax::Name::Null", "X::Syntax");
        register_x("X::Syntax::Signature", "X::Syntax");
        register_x(
            "X::Syntax::Signature::InvocantMarker",
            "X::Syntax::Signature",
        );

        // X::Obsolete (compile-time, subtype of X::Comp)
        register_x("X::Obsolete", "X::Comp");

        // X::Undeclared hierarchy
        register_x("X::Undeclared", "X::Comp");
        register_x("X::Undeclared::Symbols", "X::Comp");

        // X::Redeclaration
        register_x("X::Redeclaration", "X::Comp");

        // X::Assignment::RO
        register_x("X::Assignment::RO", "Exception");

        // X::Str::Numeric
        register_x("X::Str::Numeric", "Exception");

        // X::Str::Match::x — invalid :x argument to .subst / s///
        register_x("X::Str::Match::x", "Exception");

        // X::Multi::NoMatch / X::Multi::Ambiguous
        register_x("X::Multi::NoMatch", "Exception");
        register_x("X::Multi::Ambiguous", "Exception");

        // X::OutOfRange
        register_x("X::OutOfRange", "Exception");

        // X::Method::NotFound
        register_x("X::Method::NotFound", "Exception");

        // X::Immutable
        register_x("X::Immutable", "Exception");

        // X::Cannot::Lazy
        register_x("X::Cannot::Lazy", "Exception");
        register_x("X::Cannot::Capture", "Exception");

        // X::Match::Bool
        register_x("X::Match::Bool", "Exception");

        // X::Adverb
        register_x("X::Adverb", "Exception");

        // X::ControlFlow::Return
        register_x("X::ControlFlow::Return", "Exception");

        // X::Bind
        register_x("X::Bind", "Exception");
        register_x("X::Bind::NativeType", "X::Bind");

        // X::StubCode
        register_x("X::StubCode", "Exception");

        // X::Signature::Placeholder
        register_x("X::Signature::Placeholder", "Exception");

        // X::Signature::NameClash
        register_x("X::Signature::NameClash", "X::Comp");

        // X::SecurityPolicy
        register_x("X::SecurityPolicy", "Exception");

        // X::NotEnoughDimensions
        register_x("X::NotEnoughDimensions", "Exception");

        // X::IO::Closed
        register_x("X::IO::Closed", "Exception");

        // X::Role subtypes
        register_x("X::Role::Parametric::NoSuchCandidate", "Exception");
        register_x("X::Role::Unimplemented::Multi", "Exception");

        // X::NYI
        register_x("X::NYI", "Exception");

        // X::Method::Private::Permission
        register_x("X::Method::Private::Permission", "Exception");
        register_x("X::Method::Private::Unqualified", "Exception");
        register_x("X::Routine::Unwrap", "Exception");
        register_x("X::Str::Trans::InvalidArg", "Exception");
        register_x("X::Str::Trans::IllegalKey", "Exception");

        // X::ParametricConstant
        register_x("X::ParametricConstant", "Exception");

        // X::UnitScope::Invalid
        register_x("X::UnitScope::Invalid", "Exception");

        // X::Promise / X::Channel exceptions
        register_x("X::Promise::Vowed", "Exception");
        register_x("X::Promise::Resolved", "Exception");
        register_x("X::Promise::CauseOnlyValidOnBroken", "Exception");
        register_x("X::Channel::SendOnClosed", "Exception");
        register_x("X::Channel::ReceiveOnClosed", "Exception");

        // X::Comp::AdHoc does both X::Comp and X::AdHoc in rakudo (it is the
        // compile-time wrapper around an ad-hoc die), so `$e ~~ X::AdHoc` must
        // also be True. register_x only threads a single parent, so splice
        // X::AdHoc into the MRO here (after the closure's borrow of `classes`
        // has ended).
        if let Some(def) = classes.get_mut("X::Comp::AdHoc") {
            if !def.parents.iter().any(|p| p == "X::AdHoc") {
                def.parents.push("X::AdHoc".to_string());
            }
            if !def.mro.iter().any(|m| m == "X::AdHoc") {
                let insert_at = def.mro.len().saturating_sub(1);
                def.mro.insert(insert_at, "X::AdHoc".to_string());
            }
        }

        let mut interpreter = Self {
            env: Env::from(env),
            output_sink: Arc::new(RwLock::new(OutputSink::new())),
            warn_output: String::new(),
            warn_suppression_depth: 0,
            tap: TapState::default(),
            halted: false,
            exit_code: 0,
            nested_mode: false,
            native_call_specs: HashMap::new(),
            operator_assoc: HashMap::new(),
            imported_operator_names: HashSet::new(),
            lib_paths: Vec::new(),
            io_handles: Arc::new(RwLock::new(io_handles::IoHandleTable {
                map: HashMap::new(),
                next_id: 1,
            })),
            program_path: None,
            current_package: Arc::new(RwLock::new("GLOBAL".to_string())),
            routine_stack: Vec::new(),
            callframe_stack: Vec::new(),
            method_class_stack: Vec::new(),
            constructing_class: None,
            pending_call_arg_sources: None,
            test_pending_callsite_line: None,
            control_handler_depth: 0,
            test_assertion_line_stack: Vec::new(),
            block_stack: Vec::new(),
            doc_comments: HashMap::new(),
            doc_comment_list: Vec::new(),
            why_cache: HashMap::new(),
            type_metadata: HashMap::new(),
            when_matched: false,
            gather_items: Vec::new(),
            gather_take_limits: Vec::new(),
            block_scope_depth: 0,
            registry: {
                // Built-in class definitions (PR-A slice 3: `classes` now lives in the
                // shared Registry instead of an Interpreter field).
                let mut registry = Registry {
                    classes,
                    ..Registry::default()
                };
                // Built-in class -> composed-role seeds (PR-A slice 2: class metadata
                // now lives in the shared Registry instead of an Interpreter field).
                let ccr = &mut registry.class_composed_roles;
                ccr.insert(
                    "CompUnit::Repository::FileSystem".to_string(),
                    vec!["CompUnit::Repository".to_string()],
                );
                // Built-in type role composition
                ccr.insert(
                    "Int".to_string(),
                    vec!["Real".to_string(), "Numeric".to_string()],
                );
                ccr.insert(
                    "Num".to_string(),
                    vec!["Real".to_string(), "Numeric".to_string()],
                );
                ccr.insert(
                    "Rat".to_string(),
                    vec![
                        "Rational[Int,Int]".to_string(),
                        "Real".to_string(),
                        "Numeric".to_string(),
                    ],
                );
                ccr.insert(
                    "FatRat".to_string(),
                    vec![
                        "Rational[Int,Int]".to_string(),
                        "Real".to_string(),
                        "Numeric".to_string(),
                    ],
                );
                ccr.insert("Complex".to_string(), vec!["Numeric".to_string()]);
                ccr.insert("Str".to_string(), vec!["Stringy".to_string()]);
                // Built-in role definitions (PR-A slice 4: roles now live in the
                // shared Registry instead of an Interpreter field).
                registry.roles = {
                    let mut roles = HashMap::new();
                    roles.insert(
                        "Encoding".to_string(),
                        RoleDef {
                            attributes: Vec::new(),
                            methods: HashMap::new(),
                            is_stub_role: false,
                            is_hidden: false,
                            is_rw: false,
                            captured_env: None,
                            wildcard_handles: Vec::new(),
                            role_id: 0,
                            attribute_conflicts: Vec::new(),
                            own_attribute_names: std::collections::HashSet::new(),
                            deferred_body_stmts: Vec::new(),
                            deferred_custom_traits: Vec::new(),
                        },
                    );
                    roles.insert(
                        "Iterator".to_string(),
                        RoleDef {
                            attributes: Vec::new(),
                            methods: HashMap::new(),
                            is_stub_role: false,
                            is_hidden: false,
                            is_rw: false,
                            captured_env: None,
                            wildcard_handles: Vec::new(),
                            role_id: 0,
                            attribute_conflicts: Vec::new(),
                            own_attribute_names: std::collections::HashSet::new(),
                            deferred_body_stmts: Vec::new(),
                            deferred_custom_traits: Vec::new(),
                        },
                    );
                    roles.insert(
                        "PredictiveIterator".to_string(),
                        RoleDef {
                            attributes: Vec::new(),
                            methods: HashMap::new(),
                            is_stub_role: false,
                            is_hidden: false,
                            is_rw: false,
                            captured_env: None,
                            wildcard_handles: Vec::new(),
                            role_id: 0,
                            attribute_conflicts: Vec::new(),
                            own_attribute_names: std::collections::HashSet::new(),
                            deferred_body_stmts: Vec::new(),
                            deferred_custom_traits: Vec::new(),
                        },
                    );
                    roles.insert(
                        "Iterable".to_string(),
                        RoleDef {
                            attributes: Vec::new(),
                            methods: HashMap::new(),
                            is_stub_role: false,
                            is_hidden: false,
                            is_rw: false,
                            captured_env: None,
                            wildcard_handles: Vec::new(),
                            role_id: 0,
                            attribute_conflicts: Vec::new(),
                            own_attribute_names: std::collections::HashSet::new(),
                            deferred_body_stmts: Vec::new(),
                            deferred_custom_traits: Vec::new(),
                        },
                    );
                    roles.insert(
                        "X::Control".to_string(),
                        RoleDef {
                            attributes: Vec::new(),
                            methods: HashMap::new(),
                            is_stub_role: false,
                            is_hidden: false,
                            is_rw: false,
                            captured_env: None,
                            wildcard_handles: Vec::new(),
                            role_id: 0,
                            attribute_conflicts: Vec::new(),
                            own_attribute_names: std::collections::HashSet::new(),
                            deferred_body_stmts: Vec::new(),
                            deferred_custom_traits: Vec::new(),
                        },
                    );
                    // CompUnit::Repository role with required stub methods
                    {
                        let stub_body = vec![Stmt::Expr(Expr::Call {
                            name: Symbol::intern("__mutsu_stub_die"),
                            args: vec![],
                        })];
                        let stub_method = |body: Vec<Stmt>| MethodDef {
                            params: Vec::new(),
                            param_defs: Vec::new(),
                            body: std::sync::Arc::new(body),
                            is_rw: false,
                            is_private: false,
                            is_multi: false,
                            is_my: false,
                            role_origin: None,
                            original_role: None,
                            return_type: None,
                            compiled_code: None,
                            delegation: None,
                            is_default: false,
                            deprecated_message: None,
                            is_submethod: false,
                        };
                        let mut methods = HashMap::new();
                        for name in ["id", "need", "load", "loaded"] {
                            methods.insert(name.to_string(), vec![stub_method(stub_body.clone())]);
                        }
                        roles.insert(
                            "CompUnit::Repository".to_string(),
                            RoleDef {
                                attributes: Vec::new(),
                                methods,
                                is_stub_role: false,
                                is_hidden: false,
                                is_rw: false,
                                captured_env: None,
                                wildcard_handles: Vec::new(),
                                role_id: 0,
                                attribute_conflicts: Vec::new(),
                                own_attribute_names: std::collections::HashSet::new(),
                                deferred_body_stmts: Vec::new(),
                                deferred_custom_traits: Vec::new(),
                            },
                        );
                    }
                    // `Distribution` built-in interface role. Real Rakudo defines
                    // it with required stub methods `meta` and `content`; user
                    // distribution classes (e.g. `Zef::Distribution does
                    // Distribution`) supply the implementations. Registering the
                    // role lets such classes compose and lets `~~ Distribution`
                    // recognize them.
                    {
                        let stub_body = vec![Stmt::Expr(Expr::Call {
                            name: Symbol::intern("__mutsu_stub_die"),
                            args: vec![],
                        })];
                        let stub_method = |body: Vec<Stmt>| MethodDef {
                            params: Vec::new(),
                            param_defs: Vec::new(),
                            body: std::sync::Arc::new(body),
                            is_rw: false,
                            is_private: false,
                            is_multi: false,
                            is_my: false,
                            role_origin: None,
                            original_role: None,
                            return_type: None,
                            compiled_code: None,
                            delegation: None,
                            is_default: false,
                            deprecated_message: None,
                            is_submethod: false,
                        };
                        let mut methods = HashMap::new();
                        for name in ["meta", "content"] {
                            methods.insert(name.to_string(), vec![stub_method(stub_body.clone())]);
                        }
                        roles.insert(
                            "Distribution".to_string(),
                            RoleDef {
                                attributes: Vec::new(),
                                methods,
                                is_stub_role: false,
                                is_hidden: false,
                                is_rw: false,
                                captured_env: None,
                                wildcard_handles: Vec::new(),
                                role_id: 0,
                                attribute_conflicts: Vec::new(),
                                own_attribute_names: std::collections::HashSet::new(),
                                deferred_body_stmts: Vec::new(),
                                deferred_custom_traits: Vec::new(),
                            },
                        );
                    }
                    roles
                };
                Arc::new(RwLock::new(registry))
            },
            proto_dispatch_stack: Vec::new(),
            proto_method_skip: None,
            pending_dispatch_error: None,
            end_phasers: Vec::new(),
            end_phaser_sites: HashSet::new(),
            chroot_root: None,
            loaded_modules: HashSet::new(),
            need_hidden_classes: HashSet::new(),
            cur_repo: Box::new(CurRepoState::default()),
            package_stash_hidden: HashSet::new(),
            chain_declared_packages: HashSet::new(),
            module_packages: HashMap::new(),
            closure_env_overrides: HashMap::new(),
            predictive_seq_iters: HashMap::new(),
            protect_block_cache: HashMap::new(),
            subset_predicate_cache: HashMap::new(),
            private_zeroarg_method_cache: HashMap::new(),
            module_load_stack: Vec::new(),
            current_distribution: None,
            package_distributions: HashMap::new(),
            exported_subs: HashMap::new(),
            exported_sub_values: HashMap::new(),
            exported_vars: HashMap::new(),
            unit_module_exported_subs: HashMap::new(),
            unit_module_loading_stack: Vec::new(),
            suppress_exports: false,
            in_lvalue_assignment: false,
            in_does_rhs: false,
            trait_mod_writeback_key: None,
            trait_mod_writeback_value: None,
            hash_autovivify: false,
            newline_mode: NewlineMode::Lf,
            import_scope_stack: Vec::new(),
            strict_mode: false,
            fatal_mode: false,
            our_vars: HashMap::new(),
            package_lexicals: HashMap::new(),
            state_vars: HashMap::new(),
            closure_captured_state: HashMap::new(),
            once_values: HashMap::new(),
            once_scope_stack: Vec::new(),
            next_once_scope_id: 1,
            var_dynamic_flags: HashMap::new(),
            caller_env_stack: Vec::new(),
            var_bindings: HashMap::new(),
            variables_pragma: String::new(),
            attributes_pragma: String::new(),
            var_type_constraints: HashMap::new(),
            atomic_var_seen: false,
            var_defaults: HashMap::new(),
            var_hash_key_constraints: HashMap::new(),
            instance_type_metadata: Arc::new(RwLock::new(HashMap::new())),
            let_saves: Vec::new(),
            supply_emit_buffer: Vec::new(),
            supply_emit_timed_buffer: Vec::new(),
            supply_stream_consumers: Vec::new(),
            shared_vars: Arc::new(RwLock::new(HashMap::new())),
            shared_vars_active: false,
            sigilless_attrs_active: false,
            shared_vars_dirty: Arc::new(RwLock::new(HashSet::new())),
            encoding_registry: Self::builtin_encodings(),
            skip_pseudo_method_native: None,
            dispatch_ambiguous: false,
            pending_proxy_subclass_attr: None,
            multi_dispatch_stack: Vec::new(),
            method_dispatch_stack: Vec::new(),
            samewith_context_stack: Vec::new(),
            wrap_chains: HashMap::new(),
            wrap_sub_names: HashMap::new(),
            wrap_name_to_sub: HashMap::new(),
            wrap_callable_ids: HashMap::new(),
            wrap_handle_counter: 0,
            wrap_dispatch_stack: Vec::new(),
            method_wrap_chains: HashMap::new(),
            suppressed_names: HashSet::new(),
            poisoned_enum_aliases: HashMap::new(),
            enum_scope_names: vec![Vec::new()],
            my_scoped_package_items: HashSet::new(),
            lexical_class_scopes: Vec::new(),
            last_value: None,
            pending_local_updates: Vec::new(),
            readonly_vars: HashSet::new(),
            squish_iterator_meta: HashMap::new(),
            custom_type_data: HashMap::new(),
            rebless_map: HashMap::new(),
            action_made: None,
            current_grammar_actions: None,
            pending_regex_error: None,
            precomp_enabled: true,
            monkey_typing: false,

            // Merged VM execution registers (CP-3 collapse) — same defaults the
            // former `VM::new` installed.
            stack: Vec::new(),
            locals: Vec::new(),
            upvalues: Vec::new(),
            in_smartmatch_rhs: false,
            transliterate_in_smartmatch: false,
            substitution_in_smartmatch: false,
            last_topic_value: None,
            topic_save_stack: Vec::new(),
            container_ref_var: None,
            container_ref_reversed: false,
            topic_source_var: None,
            element_source: None,
            quanthash_bind_params: Vec::new(),
            for_param_restore_stack: Vec::new(),
            call_frames: Vec::new(),
            control_handlers: Vec::new(),
            current_code: 0,
            carrier_writes: None,
            method_dispatch_pure: false,
            resume_ip: None,
            bind_context: false,
            scalar_bind_context: false,
            bound_decont_active: false,
            rebind_context: false,
            constant_context: false,
            array_share_context: false,
            array_share_source: None,
            array_share_active: false,
            element_share_pending: false,
            explicit_initializer_context: false,
            vardecl_context: false,
            shaped_decl_context: false,
            pending_rw_writeback_sources: Vec::new(),
            pending_caller_var_writeback: Vec::new(),
            local_bind_pairs: Vec::new(),
            otf_compile_cache: HashMap::new(),
            state_scope_id: None,
            fn_resolve_cache: HashMap::new(),
            fn_resolve_gen: 0,
            fn_resolve_cache_gen: 0,
            multi_candidates_cache: HashMap::new(),
            multi_candidates_cache_gen: 0,
            light_call_cache: HashMap::new(),
            light_call_cache_gen: 0,
            pos_light_call_cache: HashMap::new(),
            pos_light_call_cache_gen: 0,
            amp_param_shadowed_names: std::collections::HashSet::new(),
            registered_fn_fingerprints: HashMap::new(),
            prepared_fn_defs: HashMap::new(),
            method_resolve_cache: rustc_hash::FxHashMap::default(),
            last_method_resolve: None,
            fast_method_cache: rustc_hash::FxHashMap::default(),
            multi_resolve_cache: rustc_hash::FxHashMap::default(),
            multi_type_cacheable: rustc_hash::FxHashMap::default(),
            dispatch_multi_candidate: rustc_hash::FxHashMap::default(),
            method_body_fp_cache: rustc_hash::FxHashMap::default(),
            block_declared_vars: Vec::new(),
            loop_local_vars: Vec::new(),
            loop_local_saved_env: Vec::new(),
            loop_cond_active: false,
            outer_scope_locals: Vec::new(),
            enter_result_stack: Vec::new(),
            pending_alias_bind_names: Vec::new(),
            otf_call_cache: HashMap::new(),
            otf_call_cache_gen: 0,
            check_phaser_depth: 0,
            gather_for_loop_resume: None,
            rw_map_topic_capture: None,
        };
        interpreter.init_io_environment();
        // Built-in enum constants (Order/Endian/ProtocolFamily/Signal) are
        // process-wide immutables: collect them into the shared base tier
        // instead of every per-frame env overlay (docs/vm-dual-store.md 4b).
        let mut enum_base: HashMap<Symbol, Value> = HashMap::new();
        interpreter.init_order_enum(&mut enum_base);
        interpreter.init_endian_enum(&mut enum_base);
        interpreter.init_protocol_family_enum(&mut enum_base);
        interpreter.init_signal_enum(&mut enum_base);
        interpreter.init_seek_type_enum(&mut enum_base);
        // Hoist the immutable process-constant magic/dynamic vars out of every
        // per-frame env overlay into the shared base tier (docs/vm-dual-store.md
        // 4c "natural extension"). These are set once at interpreter start and
        // never reassigned/removed by normal programs; reads fall back to the
        // base tier, and a rare write is promoted into the overlay by
        // `Env::get_mut`, so semantics are preserved while the per-call deep
        // copy forks a smaller overlay. Mutable dynamics ($*OUT, $*CWD, %*ENV,
        // @*ARGS, $*SCHEDULER, $*REPO, handles, ...) intentionally stay in the
        // overlay.
        for key in IMMUTABLE_BASE_DYNAMICS {
            if let Some(v) = interpreter.env.remove(key) {
                enum_base.insert(Symbol::intern(key), v);
            }
        }
        crate::env::set_global_base(enum_base);
        interpreter.env.insert("Any".to_string(), Value::Nil);
        // Set up $*REPO as a default CompUnit::Repository::FileSystem instance
        {
            let mut attrs = HashMap::new();
            attrs.insert("prefix".to_string(), Value::str_from("."));
            attrs.insert("__mutsu_precomp_enabled".to_string(), Value::Bool(true));
            let repo =
                Value::make_instance(Symbol::intern("CompUnit::Repository::FileSystem"), attrs);
            interpreter.env.insert("*REPO".to_string(), repo);
        }
        interpreter
    }
}
