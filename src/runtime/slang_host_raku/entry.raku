our $Mutsu::Slang::Host::G;
our $Mutsu::Slang::Host::Built;
our $Mutsu::Slang::Host::A;
our $Mutsu::Slang::Host::Roles;
# `$*ORIGIN-SOURCE` of a statement override: the unit the statement is in.
class Mutsu::Slang::Host::OriginSource { has $.original-file }
sub mutsu-slang-host-run(Str $rule, Str $source, @groles, @aroles, :$node, :$block, Int :$pos = 0, Int :$to, :$file, :$compose-only) {
    # The composed grammar is a type object, which is never `.defined`: key the
    # cache on the roles it was composed from, so it is built once per slang
    # registration rather than once per production.
    my $roles = (|@groles, '|', |@aroles).map(*.WHICH).join(' ');
    unless $Mutsu::Slang::Host::A.defined && $Mutsu::Slang::Host::Roles eq $roles {
        $Mutsu::Slang::Host::Roles = $roles;
        my $g = Metamodel::GrammarHOW.new_type(:name<Raku::Grammar>);
        $g.^add_parent(Mutsu::Slang::Host::Base);
        $g.^add_role($_) for @groles;
        $g.^compose;
        my $a = Metamodel::ClassHOW.new_type(:name<Raku::Actions>);
        $a.^add_parent(Mutsu::Slang::Host::Actions);
        $a.^add_role($_) for @aroles;
        $a.^compose;
        $Mutsu::Slang::Host::G = $g;
        $Mutsu::Slang::Host::A = $a.new;
    }
    return if $compose-only;
    if $node.defined {
        # A numeric literal's `$/` is the text it was written as, holding the
        # capture the core grammar's `numish` has (`integer`, `decimal-number`,
        # `radix-number`), which an override reads by key.
        my %hash;
        if $rule eq 'numish' && $source.chars {
            my $part = Match.new(:orig($source), :from(0), :to($source.chars));
            # Only an ASCII integer, decimal or radix literal has a capture; `NaN`,
            # `Inf`, `∞`, a unum (`Ⅼ`, `⅔`), `<1/2>` and `<1+2i>` are numish alone.
            my $key = $source.starts-with(':') ?? 'radix-number'
                !! $source ~~ /^ [ 0 <[xXoObB]> <[0..9a..fA..F_]>+ | <[0..9]> <[0..9_]>* ] $/ ?? 'integer'
                !! $source ~~ /^ <[0..9.]> <[0..9_.eE+\-]>* $/ ?? 'decimal-number'
                !! Nil;
            %hash{$key} = $part if $key;
        }
        # A statement's `$/` is the statement within its unit's text (`.from`,
        # `.to` and `.orig` place it); other overrides see their own text.
        my $core = Match.new(:orig($source), :from($pos), :to($to // $source.chars), :hash(%hash));
        $core.make($node);
        my $*ORIGIN-SOURCE = Mutsu::Slang::Host::OriginSource.new(:original-file($file // ''));
        $Mutsu::Slang::Host::A."$rule"($core);
        return ($core.to, $core.ast);
    }
    my $*R = Mutsu::Slang::Host::Resolver.new;
    my $*CU = Mutsu::Slang::Host::CompUnit.new;
    $Mutsu::Slang::Host::Built = Nil;
    # A wrapped statement list runs in the scope rakudo's `<block>` / `method-def`
    # entered for it: the routine whose body it is, else a bare block (ADR-16888 §2.6).
    my $m = $rule eq 'statementlist'
        ?? do {
            my $*BLOCK = $block // RakuAST::Block.new;
            $Mutsu::Slang::Host::G.subparse($source, :rule($rule), :$pos, :actions($Mutsu::Slang::Host::A))
        }
        !! $Mutsu::Slang::Host::G.subparse($source, :rule($rule), :$pos, :actions($Mutsu::Slang::Host::A));
    return Nil unless $m;
    # A node the core action built for a production the slang's own token matched
    # (a `for` with the slang's extra clause) stands for it: a slang action that
    # only `callsame`s into the core one, or that attached nothing, leaves it.
    # (A shim leaves the last core production's node on the cursor, so `.ast`
    # alone cannot tell.)
    if !$Mutsu::Slang::Host::Built.defined && !$m.ast.defined && Mutsu::Slang::Host::Actions.can($rule) {
        Mutsu::Slang::Host::Actions.new."$rule"($m);
    }
    if $Mutsu::Slang::Host::Built.defined {
        return ($m.to, $Mutsu::Slang::Host::Built);
    }
    # Constants the production installed become `my constant NAME = VALUE`
    # declarations of the unit, standing for the statement.
    if $*R.generated.elems && $m.ast.defined {
        # A type the production minted is rebuilt in the unit by replaying the
        # recorded metamodel calls (ADR-13370); other values travel as they are.
        my $log := __mutsu_slang_class_log();
        my @decls = $*R.generated.map: -> $d {
            my sub literal($value) {
                RakuAST::Term::Declaration.new(RakuAST::Declaration::ResolvedConstant.new(:compile-time-value($value)))
            }
            my $init := $log.elems && !$d.value.defined
                ?? RakuAST::Call::Name.new(
                    name => RakuAST::Name.from-identifier('__mutsu_slang_replay_class'),
                    args => RakuAST::ArgList.new(literal($log), literal($d.value)))
                !! literal($d.value);
            RakuAST::Statement::Expression.new(expression => RakuAST::VarDeclaration::Constant.new(
                name => $d.name,
                initializer => RakuAST::Initializer::Assign.new($init)))
        };
        # TODO: a production installing several constants needs a statement
        # group the hook can lower; none exists, so it is refused rather than
        # dropping all but one.
        die "slang production installed {@decls.elems} generated lexicals; only one is supported" if @decls.elems > 1;
        my $stmt := @decls.head;
        return ($m.to, RakuAST::StatementList.new($stmt));
    }
    # A package declarator that parsed its own body (ADR-17741): the unit re-creates
    # the types the hook minted outside the production's own action (a
    # `FOREIGN-LANG` grammar), then re-runs that action against its real packages,
    # and binds the lexicals the action declared from what that run imported.
    if $m.ast.defined && $rule.split(':')[0] eq any(<package-declarator package_declarator>) {
        my @events = __mutsu_slang_action_events();
        if @events.elems {
            my sub literal($value) {
                RakuAST::Term::Declaration.new(RakuAST::Declaration::ResolvedConstant.new(:compile-time-value($value)))
            }
            my $site = @events.head[3];
            my $replay = RakuAST::Statement::Expression.new(expression => RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('__mutsu_slang_replay_declarator'),
                args => RakuAST::ArgList.new(literal(__mutsu_slang_class_log(True)), literal(@aroles), literal(@events))));
            my @lexicals = $*R.imports.map(*.key).unique.map: -> $name {
                RakuAST::Statement::Expression.new(expression => RakuAST::VarDeclaration::Term.new(
                    name => RakuAST::Name.from-identifier($name),
                    initializer => RakuAST::Initializer::Bind.new(RakuAST::Call::Name.new(
                        name => RakuAST::Name.from-identifier('__mutsu_slang_declared_import'),
                        args => RakuAST::ArgList.new(literal($site), literal($name))))))
            };
            return ($m.to, RakuAST::StatementList.new($replay, |@lexicals));
        }
    }
    # An action of the slang's own production may have done more than attach a node
    # (Qwiratry's `mold` registers into a module-level registry): that happened in
    # this interpreter only, so the unit re-runs the recorded actions where the
    # production stands (ADR-17243). A class the production installed as a constant has its own replay, above.
    if $m.ast.defined && $rule.split(':')[0] eq any(<routine-declarator scope-declarator statement-control statement-prefix>) {
        my @events = __mutsu_slang_action_events();
        if @events.elems {
            my sub literal($value) {
                RakuAST::Term::Declaration.new(RakuAST::Declaration::ResolvedConstant.new(:compile-time-value($value)))
            }
            my $replay = RakuAST::Statement::Expression.new(expression => RakuAST::Call::Name.new(
                name => RakuAST::Name.from-identifier('__mutsu_slang_replay_actions'),
                args => RakuAST::ArgList.new(literal(@aroles), literal(@events))));
            return ($m.to, RakuAST::StatementList.new($replay, $m.ast));
        }
    }
    return ($m.to, $m.ast) if $m.ast.defined;
    # A production whose slang attached no node stands for the core definition it
    # read (rakudo's own `routine-declarator` action answers it): `Slang::Mosdef`
    # has no actions role at all.
    for <routine-def method-def package-def> -> $key {
        my $part = $m.hash{$key};
        return ($m.to, $part.ast) if $part.defined && $part.ast.defined;
    }
    ($m.to, $m.ast)
}
&mutsu-slang-host-run
