# The resolver and compilation unit a production reads (`$*R`, `$*CU`): the host
# resolves names at run time, so declaring a lexical is a no-op and the
# context is empty.
class Mutsu::Slang::Host::Resolver {
    has @.generated;
    # The `Declaration::Import`s a production merged, as `lexical-name => value`.
    has @.imports;
    # The packages `current-package` and `get-global` answer (ADR-17741 §2.3): in
    # the unit, the package of the statement and `GLOBAL`; on the host, a fresh
    # scratch package each, which the hook's run binds into and then drops.
    has Mu $!package;
    has Mu $!global;
    has Bool $!package-known;
    has Bool $!global-known;
    my Int $scratch-serial = 0;
    submethod BUILD(Mu :$package, Mu :$global, Bool :$known) {
        if $known {
            $!package := $package;
            $!global := $global;
            $!package-known = $!global-known = True;
        }
    }
    method !scratch() {
        Metamodel::PackageHOW.new_type(:name('Mutsu::Slang::Host::Scratch' ~ ++$scratch-serial))
    }
    method current-package() {
        unless $!package-known { $!package := self!scratch; $!package-known = True }
        $!package
    }
    method get-global() {
        unless $!global-known { $!global := self!scratch; $!global-known = True }
        $!global
    }
    method declare-lexical($declaration) { $declaration }
    method create-scope-implicits() { }
    # `$*R.current-scope.merge-generated-lexical-declaration($decl)`: a constant
    # a production installs for the unit (Qwiratry's `aggsub` class). The host
    # keeps it; the run binds it in the unit beside the statement it returns.
    # An `Import` (Grammar::BNF's `modern-install`) is a lexical the statement
    # declares, whose value the unit's run of the action supplies.
    method current-scope() { self }
    method merge-generated-lexical-declaration($declaration, :$resolver, :$force) {
        if $declaration ~~ RakuAST::Declaration::Import {
            @!imports.push: $declaration.lexical-name => $declaration.compile-time-value;
        }
        else {
            @!generated.push($declaration);
        }
    }
    # The setting's declaration of a lexical spelled exactly `$name`: a node the
    # parser resolved to the setting already carries that declaration, with
    # rakudo's spelling (`&infix:<==>`, `&infix:«<=»`); another spelling names
    # no lexical of the setting (EEL 0.3's `resolves-to-setting`, #16888).
    # TODO: answer the declaration for the exact spelling too, once a host can
    # make one (an instance of `RakuAST::Declaration::External::Setting`).
    method resolve-lexical-constant-in-setting($name) { Nil }
}
class Mutsu::Slang::Host::CompUnit { method context() { Nil } }
class Mutsu::Slang::Host::Actions {
    method typename(Mu $/) {
        $/.make(RakuAST::Type::Simple.new(RakuAST::Name.from-identifier(~$/)));
    }
    method attach($/, $ast) { $/.make($ast); $ast }
    # rakudo's declarator actions (`package-declarator:sym<class>`, ...) attach
    # the definition through it; it also widens the node's origin, which the
    # host does not track.
    method DECLARATOR($/, $ast) { self.attach($/, $ast) }
    # The core actions a slang may override and `callsame` into: the core node
    # is the Match's `.ast` already, so they answer it.
    method INFIX-EXPR(Mu $/) { $/.ast }
    method PREFIX-EXPR(Mu $/) { $/.ast }
    method POSTFIX-EXPR(Mu $/) { $/.ast }
    method numish(Mu $/) { $/.ast }
    # A statement, which a slang may override to wrap or rewrite (Dawa's
    # debugger stop points).
    method statement(Mu $/) { $/.ast }
    # The block statements a slang's actions may override.
    method statement-control:sym<given>(Mu $/) { $/.ast }
    # A `for` a slang's own token matched (its header is the shims' nodes) has
    # no core node yet; one the core parser matched is the Match's `.ast`.
    method statement-control:sym<for>(Mu $/) {
        return $/.ast unless $<EXPR>.defined && $<pointy-block>.defined;
        my $node = RakuAST::Statement::For.new(
            source => $<EXPR>.ast, body => $<pointy-block>.ast);
        $/.make($node);
        $Mutsu::Slang::Host::Built = $node;
        $node
    }
    method statement-control:sym<if>(Mu $/) { $/.ast }
    method statement-control:sym<while>(Mu $/) { $/.ast }
}
