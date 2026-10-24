//! `CompUnit::Repository::Installation`'s and `CompUnit::Repository::FileSystem`'s
//! rows (ADR-11276 §9.117).
//!
//! A repository has an opaque dispatch shape (`DispatchShape::is_opaque`), as
//! `CompUnit::Handle` does, so every call entry reaches the rows through the
//! table. Every row
//! needs the interpreter (it reads the file system, loads a module or walks the
//! repository chain); the implementations are `Interpreter` methods in
//! `runtime/methods_distribution*.rs`. A repository instance carries its
//! `prefix` (a path string) and its `next-repo` link as attributes.

use super::super::{Handler, MethodRow, Named, RowFlags};
use crate::runtime::Interpreter;
use crate::value::{AttrMap, RuntimeError, Value, ValueView};

type Answer = Option<Result<Value, RuntimeError>>;

const INST: &str = "CompUnit::Repository::Installation";
const FS: &str = "CompUnit::Repository::FileSystem";

/// A row of `$owner` reached through its owner, needing the interpreter.
macro_rules! row {
    ($owner:expr, $name:literal, $arity:literal, $handler:ident) => {
        row!(
            $owner,
            $name,
            $arity,
            $handler,
            RowFlags::ANY_ARGS.or(RowFlags::OBJECT_ARGS)
        )
    };
    ($owner:expr, $name:literal, $arity:literal, $handler:ident, slurpy) => {
        row!(
            $owner,
            $name,
            $arity,
            $handler,
            RowFlags::SLURPY
                .or(RowFlags::ANY_ARGS)
                .or(RowFlags::OBJECT_ARGS)
        )
    };
    ($owner:expr, $name:literal, $arity:literal, $handler:ident, $flags:expr) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: $arity,
            handler: Handler::Interp($handler),
            flags: $flags,
            named: named_of($name),
        }
    };
}

/// The named arguments Rakudo's repository method `name` declares.
// Cost: O(1).
const fn named_of(name: &str) -> &'static [&'static str] {
    use crate::value::const_str_eq;
    if const_str_eq(name, "candidates") {
        &["api", "auth", "ver"]
    } else if const_str_eq(name, "files") {
        &["api", "auth", "name", "ver"]
    } else if const_str_eq(name, "install") {
        &["force", "precompile"]
    } else if const_str_eq(name, "need") {
        &["precomp-stores"]
    } else {
        &[]
    }
}

pub(super) static ROWS: &[MethodRow] = &[
    row!(INST, "candidates", 1, inst_candidates, slurpy),
    row!(INST, "install", 1, inst_install, slurpy),
    row!(INST, "uninstall", 1, inst_uninstall),
    row!(INST, "installed", 0, inst_installed),
    row!(INST, "can-install", 0, inst_can_install),
    row!(INST, "path-spec", 0, inst_path_spec),
    row!(INST, "short-id", 0, inst_short_id),
    row!(INST, "Str", 0, prefix_str),
    row!(INST, "gist", 0, inst_path_spec),
    row!(INST, "id", 0, inst_id),
    row!(INST, "loaded", 0, inst_loaded),
    row!(INST, "next-repo", 0, next_repo),
    row!(INST, "name", 0, inst_name),
    row!(INST, "prefix", 0, prefix_path),
    row!(INST, "need", 1, inst_need, slurpy),
    row!(INST, "files", 1, inst_files, slurpy),
    row!(INST, "resolve", 1, resolve),
    row!(INST, "repo-chain", 0, repo_chain),
    row!(FS, "precomp-repository", 0, fs_precomp_repository),
    row!(FS, "candidates", 1, fs_candidates, slurpy),
    row!(FS, "distribution", 0, fs_distribution),
    row!(FS, "loaded", 0, fs_loaded),
    row!(FS, "id", 0, fs_id),
    row!(FS, "path-spec", 0, fs_path_spec),
    row!(FS, "short-id", 0, fs_short_id),
    row!(FS, "Str", 0, prefix_str),
    row!(FS, "gist", 0, fs_path_spec),
    row!(FS, "files", 1, fs_files, slurpy),
    row!(FS, "load", 1, fs_load),
    row!(FS, "need", 1, fs_need, slurpy),
    row!(FS, "next-repo", 0, next_repo),
    row!(FS, "prefix", 0, prefix_path),
    row!(FS, "resolve", 1, resolve),
    row!(FS, "repo-chain", 0, repo_chain),
];

/// A copy of the repository's attributes (a handler may write the `next-repo`
/// link back while it reads them) and its prefix.
// Cost: O(k), k = attributes of the instance.
fn repo(target: &Value) -> Option<(AttrMap, String)> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    let attributes = attributes.as_map().clone();
    let prefix = attributes
        .get("prefix")
        .map(Value::to_string_value)
        .unwrap_or_default();
    Some((attributes, prefix))
}

/// The prefix alone.
// Cost: O(1).
fn prefix_of(target: &Value) -> Option<String> {
    let ValueView::Instance { attributes, .. } = target.view() else {
        return None;
    };
    Some(
        attributes
            .as_map()
            .get("prefix")
            .map(Value::to_string_value)
            .unwrap_or_default(),
    )
}

/// The first positional argument, `Nil` when there is none.
// Cost: O(1).
fn first(args: &[Value]) -> Value {
    args.first().cloned().unwrap_or(Value::NIL)
}

/// `.candidates($spec)`.
// Cost: O(d), d = distributions installed under the prefix.
fn inst_candidates(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let prefix = prefix_of(t)?;
    Some(i.cur_inst_candidates(&prefix, &first(args)))
}

/// `.install(Distribution $distribution, Bool :$force)`.
// Cost: O(f), f = files of the distribution.
fn inst_install(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    let prefix = prefix_of(t)?;
    let force = named.get("force").is_some_and(Value::truthy);
    Some(i.cur_inst_install_checked(&prefix, &first(args), false, force))
}

/// `.uninstall(Distribution $distribution)`.
// Cost: O(f), f = files of the distribution.
fn inst_uninstall(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let prefix = prefix_of(t)?;
    Some(i.cur_inst_install_checked(&prefix, &first(args), true, false))
}

/// `.installed`.
// Cost: O(d), d = distributions installed under the prefix.
fn inst_installed(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let prefix = prefix_of(t)?;
    Some(i.cur_inst_installed_with_core(&prefix))
}

/// `.can-install`.
// Cost: O(p), p = components of the prefix.
fn inst_can_install(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let prefix = prefix_of(t)?;
    Some(Ok(Value::truth(Interpreter::cur_inst_can_install(&prefix))))
}

/// `.path-spec` and `.gist`: `inst#<prefix>`, which `.say for
/// $*REPO.repo-chain` prints.
// Cost: O(p), p = chars of the prefix.
fn inst_path_spec(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Value::str(format!("inst#{}", prefix_of(t)?))))
}

/// `.short-id`.
// Cost: O(1).
fn inst_short_id(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    prefix_of(t)?;
    Some(Ok(Value::str_from("inst")))
}

/// `.Str`: the bare prefix.
// Cost: O(1).
fn prefix_str(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Value::str(prefix_of(t)?)))
}

/// `.prefix`: the prefix as an `IO::Path`.
// Cost: O(1).
fn prefix_path(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let prefix = prefix_of(t)?;
    Some(Ok(i.make_io_path_instance(&prefix)))
}

/// `.id`.
// Cost: O(p), p = chars of the prefix.
fn inst_id(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Value::str(Interpreter::cur_inst_id(&prefix_of(t)?))))
}

/// `.loaded`.
// Cost: O(l), l = compunits the repository loaded.
fn inst_loaded(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let prefix = prefix_of(t)?;
    Some(Ok(i.cur_inst_loaded(&prefix)))
}

/// `.name`: the name the repository is registered under.
// Cost: O(r), r = registered and built-in repository names.
fn inst_name(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    repo(t)?;
    Some(Ok(i.repo_registry_name_for(t)))
}

/// `.next-repo`.
// Cost: O(k), k = attributes of the instance.
fn next_repo(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let (attributes, _) = repo(t)?;
    Some(Ok(i.repo_next_link(t, &attributes)))
}

/// `.repo-chain`: the repository and every one its `next-repo` links reach.
// Cost: O(c), c = repositories in the chain.
fn repo_chain(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    repo(t)?;
    Some(Ok(i.repo_chain(t)))
}

/// `.need($spec)`.
// Cost: the cost of loading the module.
fn inst_need(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let prefix = prefix_of(t)?;
    Some(i.cur_inst_need(&prefix, t, args.first().cloned()))
}

/// `.files($file)`.
// Cost: O(d), d = distributions installed under the prefix.
fn inst_files(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let prefix = prefix_of(t)?;
    let search_path = first(args).to_string_value();
    Some(Ok(i.cur_inst_files(&prefix, &search_path)))
}

/// `.resolve($spec)`: the best candidate as a `CompUnit`, or the next
/// repository's answer.
// Cost: O(d * c), d = candidate distributions, c = repositories in the chain.
fn resolve(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let ValueView::Instance { class_name, .. } = t.view() else {
        return None;
    };
    let (attributes, _) = repo(t)?;
    Some(i.cur_resolve(t, &class_name.resolve(), &attributes, first(args)))
}

/// `.precomp-repository`: mutsu loads modules from source (precompilation is a
/// parse-cache detail), so this is the repository object the CompUnit API
/// promises, which precomp-aware callers (roast's `Test::Compile`) thread
/// through `need`.
// Cost: O(1).
fn fs_precomp_repository(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    prefix_of(t)?;
    Some(Ok(Value::make_instance(
        crate::symbol::Symbol::intern("CompUnit::PrecompilationRepository::Default"),
        std::collections::HashMap::new(),
    )))
}

/// `.candidates($spec)`.
// Cost: O(m), m = modules the repository provides.
fn fs_candidates(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let prefix = prefix_of(t)?;
    Some(i.cur_fs_candidates(&prefix, &first(args)))
}

/// `.distribution`.
// Cost: O(m), m = modules the repository provides.
fn fs_distribution(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let prefix = prefix_of(t)?;
    Some(i.cur_fs_distribution(&prefix))
}

/// `.loaded`.
// Cost: O(l), l = compunits the repository loaded.
fn fs_loaded(i: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    let prefix = prefix_of(t)?;
    Some(Ok(Value::array(i.cur_repo_loaded(&prefix))))
}

/// `.id`.
// Cost: O(f), f = files under the prefix.
fn fs_id(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Value::str(Interpreter::cur_fs_id(&prefix_of(t)?))))
}

/// `.path-spec` and `.gist`: `file#<prefix>`.
// Cost: O(p), p = chars of the prefix.
fn fs_path_spec(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    Some(Ok(Value::str(format!("file#{}", prefix_of(t)?))))
}

/// `.short-id`.
// Cost: O(1).
fn fs_short_id(_: &mut Interpreter, t: &Value, _: &[Value], _: Named<'_>) -> Answer {
    prefix_of(t)?;
    Some(Ok(Value::str_from("file")))
}

/// `.files($file, :$name, :$auth, :$ver, :$api)`.
// Cost: O(m), m = modules the repository provides.
fn fs_files(i: &mut Interpreter, t: &Value, args: &[Value], named: Named<'_>) -> Answer {
    let prefix = prefix_of(t)?;
    let mut all = args.to_vec();
    all.extend(named.pairs().iter().cloned());
    Some(i.cur_fs_files(&prefix, &all))
}

/// `.load($file)`.
// Cost: the cost of compiling the file.
fn fs_load(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let (attributes, _) = repo(t)?;
    Some(i.cur_fs_load(t, &attributes, first(args)))
}

/// `.need($spec)`.
// Cost: the cost of loading the module.
fn fs_need(i: &mut Interpreter, t: &Value, args: &[Value], _: Named<'_>) -> Answer {
    let (attributes, _) = repo(t)?;
    Some(i.cur_fs_need(t, &attributes, first(args)))
}
