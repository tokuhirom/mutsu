my role Tuxic {
    token routine-declarator:sym<sub> {
        <.routine-sub>
        <.end-keyword>?
        <routine-def=.key-origin('routine-def', 'sub')>
    }

    token term:sym<identifier> {
        :my $pos;
        <identifier>
        <!{
            my $ident = ~$<identifier>;
            $ident eq 'sub'|'if'|'elsif'|'while'|'until'|'for'
              || $*R.is-identifier-type([$ident])
        }>
        <?before <.unspace>|\s*'('> \s* <![:]>
        { $pos := $/.CURSOR.pos }
        <args>
#        {    # XXX not yet supported in RakuAST
#            self.add-mystery(
#              $<identifier>, $<args>.from, $<args>.Str.substr(0,1)
#            )
#        }
    }

    token methodop(Mu $*DOTTY) {
        [
          | <longname>
            {
                self.malformed("class-qualified postfix call")
                  if ~$<longname> eq '::';
            }

          | <?[$@&]>
            <variable>
            { self.check-variable($<variable>) }

          | <?['"]>
            [ <!{$*QSIGIL}> || <!before '"' <-["]>*? [\s|$] > ] # dwim on "$foo."
            <quote>
            [ <?before '(' | '.(' | '\\'>
                || <.panic: "Quoted method name requires parenthesized arguments. If you meant to concatenate two strings, use '~'.">
            ]
            <.dotty-non-ident($*DOTTY)>
        ] \s* <.unspace>?
        [
          [
            |  <?before  \s*'('>  \s* <args>
            | ':' <?before \s | '{'> <!{ $*QSIGIL }> <args=.arglist>
          ]
          || <!{ $*QSIGIL }> <?>
          || <?{ $*QSIGIL }> <?[.]> <?>
        ] <.unspace>?
    }
}

my role Tuxic::Legacy {
    use NQPHLL:from<NQP>;

    token routine_declarator:sym<sub> {
        :my $*LINE_NO := HLL::Compiler.lineof(self.orig(), self.from(), :cache(1));
        <sym>
        <.end_keyword>?
        <routine_def('sub')>
    }

    token term:sym<identifier> {
        :my $pos;
        <identifier>
        <!{
            my $ident = ~$<identifier>;
            $ident eq 'sub'|'if'|'elsif'|'while'|'until'|'for' || $*W.is_type([$ident])
        }>
        <?before <.unsp>|\s*'('> \s* <![:]>
        { $pos := $/.CURSOR.pos }
        <args>
        {
            self.add_mystery(
              $<identifier>, $<args>.from, $<args>.Str.substr(0,1)
            )
        }
    }

    token methodop {
        [
          | <longname>
          | <?[$@&]>
            <variable>
            { self.check_variable($<variable>) }

          | <?['"]>
            [ <!{$*QSIGIL}> || <!before '"' <-["]>*? [\s|$] > ] # dwim on "$foo."
            <quote>
            [ <?before '(' | '.(' | '\\'>
                || <.panic: "Quoted method name requires parenthesized arguments. If you meant to concatenate two strings, use '~'.">
            ]
        ] \s* <.unsp>?
        [
          [
            |  <?before  \s*'('>  \s* <args>
            | ':' <?before \s | '{'> <!{ $*QSIGIL }> <args=.arglist>
          ]
          || <!{ $*QSIGIL }> <?>
          || <?{ $*QSIGIL }> <?[.]> <?>
        ] <.unsp>?
    }
}

use Slangify Tuxic, Mu, Tuxic::Legacy, Mu;

# vim: expandtab shiftwidth=4
