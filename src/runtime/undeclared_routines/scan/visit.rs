//! The [`Visit`] half of [`Scan`]: how each statement and expression feeds
//! the scan's frames, declarations and calls.

use super::*;

impl<'ast> Visit<'ast> for Scan {
    /// Every statement list below the unit's top level is a lexical scope of
    /// its own.
    fn visit_stmts(&mut self, body: &'ast [Stmt]) {
        let parent = self.current;
        self.frames.push(Frame {
            parent,
            routines: HashSet::new(),
            opaque_imports: false,
        });
        self.current = Some(self.frames.len() - 1);
        self.package_frames.frame_opened(body, self.frames.len() - 1);
        self.terms.push(HashSet::new());
        walk_stmts(self, body);
        self.terms.pop();
        self.current = parent;
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        self.package_frames.enter_decl(stmt);
        if let Stmt::Package { name, .. } = stmt {
            for parent in crate::qualified::package_ancestors(*name).skip(1) {
                self.declared.insert(parent.as_str().to_owned());
            }
        }
        // The `EVAL` scan keeps walking after a bail: its declarations also
        // feed the undeclared-name check (`scope_blind_declared_names`).
        if self.bail && !self.eval {
            return;
        }
        match stmt {
            Stmt::SetLine(n, _) => self.line = *n,
            Stmt::Use { module, .. } if !self.eval => {
                if !conditional::imports_no_routines(module) {
                    // A `use` below the unit's top level imports into its own
                    // statement list only: the calls there are not judged, the
                    // rest of the unit still is (#13441).
                    match self.current {
                        Some(frame) => self.frames[frame].opaque_imports = true,
                        None => self.bail = true,
                    }
                }
            }
            Stmt::No { .. } | Stmt::Need { .. } | Stmt::Import { .. } if !self.eval => {
                self.bail = true;
            }
            // Dynamically-named sub: the declared name is unknowable.
            Stmt::SubDecl {
                name_expr: Some(_), ..
            } => self.bail = true,
            // The routine's own name belongs to the enclosing scope; the
            // walk reports it again inside the body, where it is harmless
            // (a routine can call itself).
            Stmt::SubDecl {
                name, is_export, ..
            } => {
                if *is_export {
                    self.declare_unit_wide_routine(name.as_str());
                } else {
                    self.declare_routine(name.as_str());
                }
                self.walk_routine_body(stmt);
            }
            Stmt::ProtoDecl { name, .. } => {
                // A top-level `proto` stays a plain declaration (not a
                // suggestion candidate), as it always was.
                if self.current.is_some() {
                    self.declare_routine(name.as_str());
                } else {
                    self.declare(name.as_str());
                }
                self.proto_name = Some(name.to_string());
                self.walk_routine_body(stmt);
            }
            Stmt::MethodDecl { .. } => self.walk_routine_body(stmt),
            // A package body's run-time half runs in the declaration's scope.
            Stmt::PackageRuntimeBody {
                name, decl, body, ..
            } => match self.package_frames.body_frame(*decl, *name) {
                Some(frame) => {
                    let saved = self.current.replace(frame);
                    self.visit_stmts(body);
                    self.current = saved;
                }
                None => walk_stmt(self, stmt),
            },
            // A parser desugaring (`sub a {...} given 3`) is no lexical scope.
            Stmt::SyntheticBlock(body) => walk_stmts(self, body),
            // The keys of an enum are terms from the declaration on.
            Stmt::EnumDecl { variants, .. } => {
                // A computed body (`(:30SLA, slip <SLB>)`) holds its keys in the
                // `__DYNAMIC__` entry: the shared helper reads them out.
                for key in crate::parser::enum_value_names(variants) {
                    self.declare_term(&key);
                    self.enum_keys.push(key);
                }
                walk_stmt(self, stmt);
                self.enum_keys.clear();
            }
            // A bare lowercase identifier standing alone as a whole statement
            // (`dead;`) is, syntactically, the exact same "no-args routine
            // reference" `record_call` already recognizes for `Stmt::Call` --
            // rakudo's parser resolves an unrecognized bare lowercase term this
            // way and reports the same "Undeclared routine" error for it
            // (verified against `raku`: a `unit class Foo; dead` body dies with
            // "Undeclared routine:\n    dead used at line N"), where mutsu
            // previously fell through the runtime's bareword resolution to a
            // plain Str. `self` is the one common legitimate bare statement
            // this parses to (a method's own bare `self` statement) and is
            // excluded explicitly.
            Stmt::Expr(Expr::BareWord(name)) if name != "self" => self.record_call(name),
            // An explicit-invocant call (`foo($obj: ...)`) is a method call.
            Stmt::Call { args, .. } if args.iter().any(|a| matches!(a, CallArg::Invocant(_))) => {
                for a in args {
                    walk_call_arg(self, a);
                }
            }
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        // A lowercase bareword in any expression position (`say bar`,
        // `my $x = bar`) is the same "no-args routine reference" as the
        // statement form above; rakudo rejects it at CHECK time too.
        if let Expr::BareWord(name) = expr
            && name != "self"
            && !name.starts_with(|c: char| c.is_ascii_uppercase())
        {
            // A parameterized type word (`gptr[Str]`, a typed attribute's
            // implicit default over a lowercase `constant gptr = Array`) refers
            // to its base; the arguments are types, not part of the name.
            let base = match name.find('[') {
                Some(open) if name.ends_with(']') => &name[..open],
                _ => name.as_str(),
            };
            self.record_call(base);
        }
        // `&name` names the routine `name` as a value: rakudo rejects an
        // undeclared one at CHECK time like a call (`say &nosuch` is
        // "Undeclared routine"). Only a plain identifier: `&infix:<+>`,
        // `&?ROUTINE`, `&*dyn` and `&Pkg::name` resolve otherwise.
        if let Expr::CodeVar(name) = expr
            && !self.bail
            && name.starts_with(|c: char| c.is_alphabetic() || c == '_')
            && name.chars().all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | '\''))
        {
            self.code_vars.push((name.clone(), self.line));
        }
        // `$( ... )` is parsed as the `.item` of a desugared block, but is not
        // a lexical scope: a routine declared inside belongs to the enclosing one.
        if let Expr::DoBlock {
            body,
            origin: crate::ast::DoBlockOrigin::Desugar,
            ..
        } = expr
        {
            walk_stmts(self, body);
            return;
        }
        walk_expr(self, expr);
    }

    fn visit_regex_node(&mut self, node: &'ast RegexNode) {
        // `<value:sym<number>>` names a proto candidate: the parser keeps the
        // `sym<number>` tail as a subscript on the bareword `sym`, which is
        // the adverb's name, not a call to a routine called `sym`.
        if let RegexNode::Subrule {
            name,
            args: Some(args),
            ..
        }
        | RegexNode::SubruleAlias {
            name,
            args: Some(args),
            ..
        } = node
            && args.source.as_deref().is_some_and(|s| s.starts_with("sym<"))
        {
            self.visit_name(name, NameKind::Regex);
            return;
        }
        walk_regex_node(self, node);
    }

    fn visit_param(&mut self, param: &'ast ParamDef) {
        // A type capture (`::T`, `role R[::T]`, `method m(::T:)`) declares
        // the type name `T` for the rest of its scope.
        if let Some(capture) = &param.type_capture {
            self.declare(capture);
        }
        walk_param(self, param);
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        match kind {
            NameKind::Call | NameKind::UserRoutineCall => self.record_call(name),
            NameKind::SubDecl => self.declare_routine(name),
            // A `proto`'s own name, already declared in the enclosing scope.
            NameKind::Decl if self.proto_name.as_deref() == Some(name) => {
                self.proto_name = None;
                self.declare_routine(name);
            }
            NameKind::Decl if self.enum_keys.iter().any(|k| k == name) => {}
            NameKind::VarDecl if is_term_decl(name) => self.declare_term(name),
            // Declarations are collected scope-blind. Assignment targets are
            // not declarations, but suppressing calls to an assigned name is
            // the safe (false-negative) direction. A callable parameter
            // (`&f`, also a role's `role R[&f]`) is in scope as the routine
            // `f`.
            NameKind::Decl
            | NameKind::VarDecl
            | NameKind::AssignTarget
            | NameKind::Param
            | NameKind::BlockParam
            // A label (`L: for ...`) is a term in its scope (`last L`,
            // `:label(L)`).
            | NameKind::Label => self.declare(name),
            _ => {}
        }
    }
}

