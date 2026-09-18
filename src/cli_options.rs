//! The `mutsu` binary's option surface: the usage text, and how an error in
//! the option list itself is reported (ADR-0017 — rakudo's message, rakudo's
//! stream, and a *zero* exit status, because a malformed option list is not a
//! failed program).
//!
//! A module of the binary, not of the library: nothing here is part of mutsu's
//! embedding API, and the usage text is the one place that knows every flag the
//! CLI accepts. Split out of `main.rs` to keep that file inside the 500-line
//! limit the repository holds every source file to.

pub(crate) fn print_help(program: &str) {
    print!("{}", usage_text(program));
}

fn usage_text(program: &str) -> String {
    let mut out = String::new();
    macro_rules! println {
        () => { out.push('\n') };
        ($($arg:tt)*) => {{ out.push_str(&format!($($arg)*)); out.push('\n'); }};
    }
    println!("Usage: {} [OPTIONS] [FILE | -e CODE]", program);
    println!();
    println!("Options:");
    println!("  -e CODE        Evaluate CODE");
    println!("  -I PATH        Add PATH to the module search path");
    println!("  -M MODULE      use MODULE before executing program (repeatable)");
    println!("  --dump-ast     Dump the AST instead of executing");
    println!("  --dump-bytecode  Dump compiled bytecode instead of executing");
    println!("  --doc          Render Pod documentation from the source");
    println!("  --doc=module   Render it with Pod::To::[module] (only Text)");
    println!("  --repl         Start the interactive REPL");
    println!("  --no-precomp   Disable module precompilation cache");
    println!("  --profile[=FILE]");
    println!("                 Profile the run and write the JSON profile to");
    println!("                 FILE (default mutsu-prof.json), plus a summary");
    println!("                 on stderr. See docs/profiler.md");
    println!("  --profile-kind=line|routine|both");
    println!("                 Which half of the profile to report (both)");
    println!("  --profile-rate=HZ");
    println!("                 Sampling rate, 1..1000000 (1000)");
    println!("  --profile-report=json|text|both");
    println!("                 Which renderings to emit (both)");
    println!("  --profile-jit=on|off");
    println!("                 Profile with the JIT forced on or off");
    println!("  -h, --help     Show this help message");
    println!();
    println!("Environment variables:");
    println!("  MUTSULIB       Colon-separated list of module search paths");
    println!("                 (searched after -I paths, so -I takes priority;");
    println!("                 both are searched before installed modules)");
    println!("  MUTSU_PRECOMP  Set to 0 to disable the module precompilation");
    println!("                 cache, like --no-precomp but for every mutsu");
    println!("                 process a script or test harness spawns");
    println!("  MUTSU_PROFILE  Set to 1 to profile the run without a flag (the");
    println!("                 way every other mutsu instrument is armed); a");
    println!("                 summary goes to stderr, and a JSON profile too");
    println!("                 when MUTSU_PROFILE_OUT names a file.");
    println!("                 MUTSU_PROFILE_RATE / _KIND / _REPORT / _OUT");
    println!("                 match the flags above");
    println!("  MUTSU_CRASH_REPORT");
    println!("                 Set to 0 to disable the fatal-signal crash");
    println!("                 report (tmp/crash/<pid>.txt, written only when");
    println!("                 the interpreter dies of SIGSEGV and friends)");
    println!("  MUTSU_CRASH_DIR");
    println!("                 Directory to write crash reports to");
    println!("                 (default: tmp/crash)");
    out
}

/// A command-line option that cannot be negated. The message goes to *stdout*
/// and the process ends **successfully** — see `docs/adr/0017` for why an
/// option-parsing error is not a failure exit.
pub(crate) fn print_negation_error(option: &str) -> ! {
    println!("SORRY! Option '{}' cannot be negated", option);
    std::process::exit(0);
}

/// An option mutsu does not know, reported the way rakudo reports it: the
/// message and the usage text on *stderr*, exit status **0** (ADR-0017). Long
/// options are named without their `=value` part, matching
/// `Illegal option --nosucharg` for `--nosucharg=foo`.
///
/// Without this, an unknown `--switch` fell through to "this must be the
/// program file" and died with `Could not open --switch`.
pub(crate) fn illegal_option(program: &str, arg: &str) -> ! {
    if let Some(long) = arg.strip_prefix("--") {
        let name = long.split('=').next().unwrap_or(long);
        eprintln!("Illegal option --{}", name);
    } else {
        eprintln!("No such option {}", arg);
    }
    eprint!("{}", usage_text(program));
    std::process::exit(0);
}

pub(crate) fn handle_negated_short_option(
    arg: &str,
    auto_print: &mut bool,
    auto_loop: &mut bool,
) -> Option<Result<(), ()>> {
    let name = arg.strip_prefix("-/")?;
    if name.len() != 1 {
        print_negation_error(arg);
    }
    Some(match name {
        "h" | "v" => Ok(()),
        "n" => {
            *auto_loop = false;
            Ok(())
        }
        "p" => {
            *auto_print = false;
            Ok(())
        }
        _ => Err(()),
    })
}

pub(crate) fn handle_negated_long_option(
    arg: &str,
    dump_ast: &mut bool,
    doc_mode: &mut bool,
    repl_flag: &mut bool,
    no_precomp: &mut bool,
) -> Option<Result<(), ()>> {
    let name = arg.strip_prefix("--/")?;
    Some(match name {
        "help" | "version" => Ok(()),
        "dump-ast" => {
            *dump_ast = false;
            Ok(())
        }
        "doc" => {
            *doc_mode = false;
            Ok(())
        }
        "repl" => {
            *repl_flag = false;
            Ok(())
        }
        "no-precomp" => {
            *no_precomp = false;
            Ok(())
        }
        _ => Err(()),
    })
}
