unit class Template::Mustache:ver<1.2.6>:auth<zef:raku-community-modules>:api<1.2.0>;

role X[Str:D $err] is Exception {  # UNCOVERABLE
    has $.str;
    method message { "$err ❮$!str❯" }
}

class X::CannotParse does X['Unable to parse'] { }

class X::FieldNotFound does X['Field not found'] { }

class X::InheritenceLost does X['Non-override content in inheritence section'] { }

class Logger {
    # Not using an enum, because when exported it pollutes the namespace
    our constant LogLevels = <
      Fatal Error Warn Info Verbose Audit Debug Trace Trace2
    >.antipairs.hash;
    subset LogLevel of Str where { LogLevels{$_}:exists };

    class LoggersMap is Hash does Associative[Callable, LogLevel] { }
    has LoggersMap $.routines;
    has LogLevel $.level is rw;


    submethod BUILD(
        LoggersMap :$!routines = LoggersMap.new,
        Callable :$routine,
        :$level)
    {
        $!level = $level // %*ENV<TEMPLATE_MUSTACHE_LOGLEVEL>.?tclc
            // 'Error';
        for LogLevels.pairs {
            $!routines{.key} ||= .value <= LogLevels<Warn>
                ?? &warn
                !! $routine // &note;
        }
    }

    proto method log(LogLevel :$level = 'Info', |) {
        return unless LogLevels{$!level} >= LogLevels{$level};
        {*}
    }

    multi method log(Exception $e, LogLevel :$level = 'Info') {
        with $!routines{$level} {
            if .?arity // 0 == 2 {
                $_.($level, $e);
            }
            else {
                $_.(sprintf "%s: %s", $level.uc, $e);
            }
        }
        else {
            warn sprintf("Error while logging [%s]: ", $e);
        }
    }

    multi method log(LogLevel :$level = 'Info', *@msgs) {
        with $!routines{$level} {
            if .?arity // 0 == 2 {
                $_.($level, @msgs.join);
            }
            else {
                $_.(sprintf "%s: %s", $level.uc, @msgs.join);
            }
        }
        else {
            warn sprintf("Error while logging [%s]: %s", @msgs.join);
        }
    }
}

has $.extension = 'mustache';
has $.from;
has $.pragma;
has %!cache;
has $.logger handles <log>;

submethod TWEAK(:&log-routine, :$log-level) {
    $!logger //= Logger.new: :routine(&log-routine), :level($log-level);
}

#use Grammar::Tracer;
grammar Grammar {
    regex TOP {
        ^  <hunk>* (.*) $
    }

    regex hunk { (.*?) [<linetag> | <tag>] }

    token linetag { ^^ (\h*) <tag> <?{
        # XXX Very ugly to use an assertion here!
        # Interpolations do NOT affect surrounding whitespace
        $<tag>.made<type> ~~ none(<var qvar mmmvar>)
    }>\h* [\n|$] }

    token ident { <+ graph - punct> <+ graph - [\<\>\{\}\[\]&=%$]>* }
    token name { [<ident> * % '.' | ('.')] }
    proto regex tag { <...> }
    regex tag:sym<comment> { $*LEFT '!' (.*?) $*RIGHT }
    token tag:sym<var> { $*LEFT \h* <name> \h* $*RIGHT }
    token tag:sym<qvar> { $*LEFT '&' \h* <name> \h* $*RIGHT }
    token tag:sym<mmmvar> {
        <?{ $*LEFT eq '{{' and $*RIGHT eq '}}' }>

        # NB: Use $*LEFT and $*RIGHT here to force the grammar to
        # recognize that this rule is a strictly longer token than
        # sym<var>. Otherwise, '{{' might be tried before '{{{'!

        $*LEFT '{' \h* <name> \h* '}' $*RIGHT
    }
    regex tag:sym<delim> {
        $*LEFT '=' (\N*?) '=' $*RIGHT
    }
    regex tag:sym<section> {
        $*LEFT (< # ^ / \< $ >) \h* (\N*?) \h* $*RIGHT
    }
    regex tag:sym<partial> {
        $*LEFT '>' \h* (\N*?) \h* $*RIGHT
    }
}

class Actions {
    has Logger $!logger handles <log>;

    submethod BUILD (:$!logger) {
        $!logger //= Logger.new;
    }

    method TOP($/) {
        my %x = :val(''), :contents([]);
        my @frames;
        @frames.unshift: $%x;
        for $<hunk>».made.flat -> $hunk {
            if $hunk ~~ Associative and $hunk<type> eq 'section' {
                if $hunk<on> {
                    @frames[0]<contents>.push: $hunk;
                    $hunk<contents> = [];
                    @frames.unshift: $hunk;
                }
                else {
                    my $f;
                    while @frames > 1 {  # UNCOVERABLE
                        $f = @frames.shift;
                        self.log: :level<Trace>, "*****", $f.raku;
                        last if $f<val> eq $hunk<val>;
                        self.log: :level<Warn>, "Closing '$f<val>' section while looking for $hunk<val> section";
                        $f = Nil;
                    }
                    if $f {
                        # $f is the opening tag for this section
                        $f<raw-contents> = $/.substr($f<pos>, $hunk<pos> - $f<pos>);
                        $f<pos> :delete;  # Not useful outside of this parse
                    }
                    else {
                        self.log: :level<Warn>, "No start of section found for /$hunk<val>!";
                    }
                }
            }
            else {
                @frames[0]<contents>.push: $hunk;
            }
        }
        @frames[0]<contents>.push(~$0) if $0.chars;

        make %x<contents>;
    }
    method hunk($/) {
        my @x;
        @x.push(~$0) if $0.chars;
        for $<linetag tag>.grep(*.defined)».made -> $tag {
            ($tag<finalizer>:delete)() if $tag<finalizer>;
            @x.push: $tag;
        }
        make @x.Slip;
    }
    method linetag($/) {
        my $tag = $<tag>.made;
        $tag<indent> = ~$0;
        make $tag;
    }
    method name($/) {
        make $0 // ~@<ident>.join('.')
    }
    method tag:sym<delim>($/) {
        my @delim = $0.comb(/\S+/);
        @delim == 2 or X::CannotParse.new(:err<Invalid delimiters>, :str($/)).throw;
        make {
            :type<delim>,
            :val(~$0),
            :finalizer({
                self.log: :level<Trace>, "Setting DELIMS ", @delim.raku;
                ($*LEFT, $*RIGHT) = @delim;
            }),
        }
    }
    method tag:sym<comment>($/) {
        make { :type<comment>, :val(~$0) }
    }
    method tag:sym<var>($/) {
        make {
            :type<var>,
            :val(~$<name>),
            :orig(~$/),
        }
    }
    method tag:sym<qvar>($/) {
        make {
            :type<qvar>,
            :val(~$<name>),
            :orig(~$/),
        }
    }
    method tag:sym<mmmvar>($/) {
        make {
            :type<mmmvar>,
            :val(~$<name>),
            :orig(~$/),
        }
    }
    method tag:sym<section>($/) {
        make {
            :type<section>,
            :delims([$*LEFT, $*RIGHT]),
            :val(~$1.trim),
            :on($0 ne '/'),
            :inverted($0 eq '^'),
            :inherits($0 eq '<'),
            :override($0 eq '$'),
            :pos($0 eq '/' ?? $/.from !! $/.to),
        }
    }
    method tag:sym<partial>($/) {
        make { :type<partial>, :val(~$0) }
    }
}

method render(|c (
          $template,
          %context,
         :$from,
         :$pragma,
         :$extension is copy,
    Bool :$literal,
         :$log-level,
         :$logger,
    # Deprecated, please use :log-level<Warn> or higher
    Bool :$warn = False,
)) {  # UNCOVERABLE
    unless self.DEFINITE {
        # If called as Template::Mustache.render(), create an instance
        # to run with
        return self.new.render: |c;
    }

    temp $!logger = $logger if $logger;
    temp $!logger.level = $_ with $log-level // ('Warn' if $warn);

    my $froms = [];
    my $*pragmas = [];
    my %*overrides;

    if not $extension.defined {
        $extension = ($!extension if self) // 'mustache';
    }
    # Allow user to specify '.mustache' or 'mustache' as extension
    $extension = map { .starts-with('.') ?? .substr(1) !! $_ }, @$extension;
    $extension = [ $extension<> ] unless $extension ~~ Positional;

    sub push-to ($target, $_) {
        when Positional { $target.push: |$_ }
        when .defined   { $target.push:  $_ }
    }
    push-to $froms, $from;
    push-to $froms, $!from if self;
    push-to $*pragmas, $pragma;
    push-to $*pragmas, $!pragma if self;

    my $actions = Actions.new: :$!logger;

    self.log: :level<Debug>, "TEMPLATE: $template.raku()";
    self.log: :level<Debug>, "FROM: $froms.raku()";
    self.log: :level<Debug>, "PRAGMA: $*pragmas.raku()";
    self.log: :level<Debug>, "EXTENSION: $extension.raku()";
    my @parsed = get-template($template, :$literal);

    self.log: :level<Debug>, "PARSED: @parsed.raku()";
    self.log: :level<Debug>, "CONTEXT:  %context.raku()";
    return format(@parsed, [%context]);

    sub find-template-file($template, $dir is copy) {
        $dir = $dir.IO;
        $dir = $*PROGRAM.sibling($dir).resolve(:completely) if $dir.is-relative;
        $extension.map({
            $dir.add($template).extension(:0parts, $_)
        }).first(*.e);
    }

    sub get-template($template, :$delims, :$indent, :$literal) {
        my $specific = specify-template;
        my $key = "{$indent // ''}>$specific";
        my @parsed;
        if %!cache{ $key }:exists {
            @parsed = %!cache{ $key };
        }
        else {
            my $str = do given $specific {
                when IO::Path {
                    CATCH {
                        # RAKUDO: slurp throws X::Adhoc exception
                        default {
                            # Ignore it
                        }
                    }

                    .slurp;
                }

                default {
                    $_
                }
            }

            # TODO Organize the parsed results so they can be indented
            # on the fly, and move indent action into format(). That way
            # we can cache just one version of the template and use that
            # everywhere it is used.
            $str .= subst(/^^/, $indent, :g) if $indent;

            @parsed = parse-template($str, :$delims);
            %!cache{ $key } = @parsed;
        }

        self.log: :level<Trace>, "Template for '$key': ", @parsed.raku;
        return @parsed;

        sub specify-template {
            if $literal {
                # Use $template itself as the template text
                return $template
            }

            for @$froms {
                when Associative {
                    .return with $_{$template};
                }

                when any(Stringy, IO::Path) {
                    .resolve(:completely).return
                        with find-template-file($template, $_);
                }

                default {
                    self.log: :level<Warn>, "Ignoring unrecognized :from() entry $_.raku()";
                }
            }

            # Couldn't find the initial template, assume literal unless
            # explicitly prohibited
            return $template unless $literal.defined;

            self.log: :level<Debug>, "Template '$template' undefined";
            return '';
        }
    }

    sub parse-template($str, :$delims) {
        my ($*LEFT, $*RIGHT) = $delims ?? @$delims !! ( '{{', '}}' );
        Grammar.parse($str, :$actions)
            or X::CannotParse.new(:$str).throw;
        self.log: :level<Trace>, $/.made.raku;
        return @($/.made // ());
    }

    # Can't use HTML::Entity, it doesn't encode &quot; (")
    #use HTML::Entity;
    sub encode-entities($str) {
        $str.trans:
            /'&'/  => '&amp;',
            /'"'/  => '&quot;',
            /'<'/  => '&lt;',
            /'>'/  => '&gt;',
            /\xA0/ => '&nbsp;',
            ;
    }

    # TODO Track recursion depth and throw if > 100?
    proto sub format(|) {*}
    multi sub format(@val, @context) {
        self.log: :level<Trace>, "** \@ ...";
        my $j = join '', @val.map: { format($_, @context) };
        self.log: :level<Trace>, "** ... \@ $j";
        $j;
    }
    multi sub format($val, @context) { $val }
    multi sub format(%val, @context) {
        sub get(@context, %val, Bool :$section, Callable :$encode is copy) {
            my $field = %val<val>;
            sub resolve($obj) {
                if $obj ~~ Callable {
                    my $str;
                    if $obj.arity > 1 {
                        self.log: :level<Warn>, "Ignoring '$field' lambda that takes $_ args";
                        $str = '';
                    }
                    elsif $obj.arity == 1 {
                        $str = $obj(%val<raw-contents> // '');
                    }
                    else {
                        $str = $obj();
                    }
                    self.log: :level<Trace>, "#** Parsing '$str'";
                    self.log: :level<Trace>, "#** ^ with delims %val<delims>.raku()" if %val<delims>;
                    # literal: don't allow lambda return a template name
                    my @parsed = get-template($str, :delims(%val<delims>), :indent(%val<indent>), :literal);
                    my $result = format(@parsed, @context);
                    return $result, True;
                }
                else {
                    #self.log: :level<Trace>, "#** Resolve of $obj.WHAT.raku() as '$obj'";
                    return $obj, False;
                }
            }

            sub visit($context, Str:D $field) {
                if $context{$field}.defined {
                    $context{$field}
                }
                elsif $context.^can($field) {  # UNCOVERABLE
                    $context."$field"()
                }
            }

            self.log: :level<Trace>, "GET '$field' from: [$(@context.^name)]", @context.raku;
            my $result;
            my $not-found = False;
            my $lambda = False;
            if $field eq '.' {
                # Implicit iterator {{.}}
                ($result, $lambda) = resolve(@context[0]);
            }
            else {
                my @field = $field.split: '.';
                for @context.map({visit($^ctx, @field[0])}) -> $ctx {
                    # In Raku, {} and [] are falsy, but Mustache
                    # treats them as truthy
                    self.log: :level<Trace>, "#** field lookup for @field[0]: '$ctx.raku()'";
                    if $ctx ~~ Promise {
                        $result = await $ctx;
                        last;
                    }
                    elsif $ctx.defined {  # UNCOVERABLE
                        ($result, $lambda) = resolve($ctx);
                        self.log: :level<Trace>, "#** ^ result is $result.raku(), lambda $lambda.raku()";
                        last;
                    }
                }
                while $result and !$lambda and @field > 1 {  # UNCOVERABLE
                    @field.shift;
                    $result = visit($result, @field[0]);
                    self.log: :level<Trace>, "#** dot field lookup for @field[0] => $result";
                    ($result, $lambda) = resolve($result)
                }
                without $result {
                    $not-found = True;
                    if ! $section and %val<orig>
                        and pragma('KEEP-UNUSED-VARIABLES')
                    {
                        $encode = Nil;  # UNCOVERABLE
                        $result = %val<orig>;
                    }
                    else {
                        $result = '';
                    }
                }
            }
            self.log: :level<Trace>, "get($field) is '$result.raku()'";
            self.log: :level<Warn>, X::FieldNotFound.new(:str($field))
                if $not-found;
            $result = $encode(~$result) if $encode;
            return $section ?? ($result, $lambda) !! $result;
        }

        sub pragma($pragma) {
            $*pragmas.grep({ .fc eq $pragma.fc });
        }

        sub extract-overrides(@contents) {
            for @contents {
                when Associative {
                    when *.<override> {
                        %*overrides{.<val>} = .<contents>;
                    }

                    when { .<type> eq 'comment' } {
                        # Ignore
                    }

                    when *.<inherits> {
                        my @parsed = get-template
                                        .<val>,
                                        :delims(.<delims>),
                                        :indent(.<indent>),
                                        :!literal,
                                        ;

                        # Contents override parent
                        extract-overrides @parsed;
                        extract-overrides .<contents>;
                    }

                    when { .<type> eq 'partial' } {
                        my @parsed = get-template
                                        .<val>,
                                        :delims(.<delims>),
                                        :indent(.<indent>),
                                        :!literal,
                                        ;

                        extract-overrides @parsed;
                    }

                    default {
                        # Ignore
                        #self.log: :level<Warn>, X::InheritenceLost
                        #    .new: :str((.<val>).fmt('< # %s'));
                    }
                }
                when Str {
                    # Ignore
                    #self.log: :level<Warn>, X::InheritenceLost
                    #    .new: :str(($_).fmt('< $ %s'));
                }
                default {
                    # Ignore
                    #self.log: :level<Warn>, X::InheritenceLost
                    #    .new: :str((.gist).fmt('< ? %s'));
                }
            }
        }

        self.log: :level<Trace>, "** \{ %val<type>: %val<val>";
        given %val<type> {
            when 'comment' { '' }
            when 'var' { get(@context, %val, :encode(&encode-entities)) }
            when 'mmmvar' | 'qvar' { get(@context, %val) }
            when 'delim' { '' }
            when 'section' {
                my ($datum, $lambda) = get(@context, %val, :section);
                if $lambda {
                    # The section was resolved by calling a lambda, which
                    # is always considered truthy, regardless of the
                    # lambda's return value
                    return %val<inverted> ?? '' !! $datum;
                }

                if %val<inherits> {
                    temp %*overrides;

                    my @parsed = get-template
                                    %val<val>,
                                    :delims(%val<delims>),
                                    :indent(%val<indent>),
                                    :!literal,
                                    ;

                    extract-overrides %val<contents>;

                    format(@parsed, @context);
                }
                elsif %val<override> {  # UNCOVERABLE
                    format(%*overrides{%val<val>} // %val<contents>, @context);
                }
                elsif !%val<inverted> and $datum -> $_ {
                    sub section_format($datum) {
                        format(%val<contents>, ($datum, |@context));
                    }

                    when Associative {
                        # Same behavior as default (below), but must not
                        # be handled as Iterable
                        section_format $_
                    }
                    when Iterable | Positional {
                        .map(&section_format).join
                    }
                    default {
                        section_format $_
                    }
                }
                elsif %val<inverted> and !$datum {  # UNCOVERABLE
                    format(%val<contents>, @context);
                }
                else {
                    self.log: :level<Trace>, "!!! EMPTY SECTION '%val<val>'";
                    ''
                }
            }
            when 'partial' {
                self.log: :level<Trace>, "- Looking up partial for {%val<val>}";
                my @parsed = get-template
                                %val<val>,
                                :delims(%val<delims>),
                                :indent(%val<indent>),
                                :!literal,
                                ;
                self.log: :level<Trace>, "PARTIAL FORMAT DATA ", @context.raku;
                format(@parsed, @context);
            }
            default { die "Impossible format type: ", %val.raku }
        }
    }
}

# vim: expandtab shiftwidth=4
