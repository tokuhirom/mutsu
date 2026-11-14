# A slang whose `package-declarator` candidate parses its own body with a
# second, named language (ADR-17741): `wordlist NAME { some words }` declares a
# class `NAME` whose `.words` are the words of the body.
#
# The candidate does not call the core `<package-def>`, so it runs on the slang
# host. Its body is parsed by `FOREIGN-LANG` with the `WordList` language the
# EXPORT registers, whose actions mint the class. The production's own action
# then binds that class the way a package declaration does, against the
# compiler's resolver `$*R`: under the enclosing package (or `GLOBAL` for a
# leading `GLOBAL::`), with stub packages for the intermediate parts and a
# lexical for a single-part name.
use nqp;
use experimental :rakuast;

grammar WordListSlang::Words {
    token TOP  { \s* <word>+ % \s+ \s* }
    token word { <[\w-]>+ }
}

class WordListSlang::Words-actions {
    method TOP($/) {
        my @words = $<word>.map(~*);
        my Mu $type := Metamodel::ClassHOW.new_type(:name($*name));
        $type.^add_method('words', my method words(Mu:) { @words.List });
        $type.^compose;
        make $type;
    }
}

# The statement the production attaches: the declared class, as a constant.
# Its overrides keep rakudo's sink check quiet about the statement.
my class WordListSlang::Declaration is RakuAST::Term::Declaration {
    method needs-sink-call(--> False) { }
    method PERFORM-CHECK(Mu $resolver, Mu $context --> True) { }
}

# Read a capture of the production's match.
my sub capture(Mu $/, Str:D $key) { $/.hash.AT-KEY($key) }

my role WordListSlang::Grammar {
    rule package-declarator:sym<wordlist> {
        <.sym><.kok>
        :my $*name;
        <longname> { $*name := $/.hash.AT-KEY('longname').Str }
        \{
        <body=.FOREIGN-LANG('WordList', 'TOP')>
        \}
    }
}

my role WordListSlang::Actions {
    method package-declarator:sym<wordlist>(Mu $/) {
        my $resolver := $*R;
        my Mu $type := capture($/, 'body').made;
        my str @parts = capture($/, 'longname').Str.split('::');
        my str $final = @parts.pop;
        my Mu $target := do if @parts && @parts[0] eq 'GLOBAL' {
            @parts.shift;
            $resolver.get-global
        }
        else {
            $resolver.current-package
        }
        my str $prefix;
        for @parts -> str $part {
            $prefix = $prefix ?? $prefix ~ '::' ~ $part !! $part;
            my Mu $stash := $target.WHO;
            $target := do if $stash.EXISTS-KEY($part) {
                $stash.AT-KEY($part)
            }
            else {
                my Mu $stub := Metamodel::PackageHOW.new_type(:name($prefix));
                $stash.BIND-KEY($part, $stub);
                $resolver.current-scope.merge-generated-lexical-declaration(
                    RakuAST::Declaration::Import.new(
                        :lexical-name($part), :compile-time-value($stub)),
                    :$resolver, :force
                ) if $part eq $prefix;
                $stub
            }
        }
        $target.WHO.BIND-KEY($final, $type);
        unless @parts {
            $resolver.current-scope.merge-generated-lexical-declaration(
                RakuAST::Declaration::Import.new(
                    :lexical-name($final), :compile-time-value($type)),
                :$resolver, :force
            );
        }
        self.attach: $/, WordListSlang::Declaration.new(
            RakuAST::Declaration::ResolvedConstant.new(:compile-time-value($type)));
    }
}

sub EXPORT(|) {
    $*LANG.define_slang('MAIN',
        $*LANG.slang_grammar('MAIN').^mixin(WordListSlang::Grammar),
        $*LANG.slang_actions('MAIN').^mixin(WordListSlang::Actions));
    $*LANG.define_slang('WordList', WordListSlang::Words, WordListSlang::Words-actions);
    BEGIN Map.new
}
