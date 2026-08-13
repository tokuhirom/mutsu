use Cro::Connection;
use Cro::Connector;
use Cro::Message;
use Cro::Replyable;
use Cro::Service;
use Cro::Sink;
use Cro::Source;
use Cro::Transform;
use Cro::UnhandledErrorReporter;

class X::Cro::Compose::Empty is Exception {
    method message() { "Cannot compose an empty list of components" }
}
class X::Cro::Compose::InvalidType is Exception {
    has $.invalid;
    method message() {
        "Cannot compose $!invalid.^name() into a Cro pipeline; it does not " ~
        "do Cro::Source, Cro::Transform, Cro::Sink, or Cro::Connector"
    }
}
class X::Cro::Compose::SourceMustBeFirst is Exception {
    has $.source;
    method message() {
        "A Cro::Source must occur at the start of a pipeline, but " ~
        $!source.^name() ~ " is placed later"
    }
}
class X::Cro::Compose::SinkMustBeLast is Exception {
    has $.sink;
    method message() {
        "A Cro::Sink must occur at the end of a pipeline, but " ~
        $!sink.^name() ~ " is placed earlier"
    }
}
class X::Cro::Compose::Mismatch is Exception {
    has $.producer;
    has $.consumer;
    method message() {
        "Cannot compose a $!producer.^name() producing $!producer.produces.^name() " ~
        "with a $!consumer.^name() consuming a $!consumer.consumes.^name()"
    }
}
class X::Cro::Compose::BadProducer is Exception {
    has $.producer;
    method message() {
        "Cannot compose $!producer.^name(); it produces $!producer.produces.^name() " ~
        "which is neither a Cro::Message nor a Cro::Connection"
    }
}
class X::Cro::Compose::BadConsumer is Exception {
    has $.consumer;
    method message() {
        "Cannot compose $!consumer.^name(); it consumes $!consumer.consumes.^name() " ~
        "which is neither a Cro::Message nor a Cro::Connection"
    }
}
class X::Cro::Compose::BadReplier is Exception {
    has $.replyable;
    method message() {
        my $replier = $!replyable.replier;
        "Replyable component $!replyable.^name() has a replier method returning a " ~
        "$replier.^name(), which is neither a Cro::Transform nor a Cro::Sink"
    }
}
class X::Cro::Compose::TooManySinks is Exception {
    has $.replyable;
    method message() {
        "Cannot apply sink $!replyable.replier.^name() from $!replyable.^name() " ~
        "because this pipeline already has a sink"
    }
}
class X::Cro::Compose::OnlyOneConnector is Exception {
    method message() {
        "Cannot compose a pipeline with more than one connector"
    }
}
class X::Cro::Compose::SourceAndConnector is Exception {
    method message() {
        "A pipeline with a connector may not also have a source"
    }
}
class X::Cro::Compose::SinkAndConnector is Exception {
    method message() {
        "A pipeline with a connector may not also have a sink"
    }
}
class X::Cro::Compose::ConnectionConditionalWithoutConnector is Exception {
    method message() {
        "Can only use a Cro::ConnectionConditional in a pipeline with a connector"
    }
}
class X::Cro::Compose::ConnectionStateWithoutConnection is Exception {
    method message() {
        "Can only compose a component doing Cro::ConnectionState in the scope of a connection"
    }
}
class X::Cro::ConnectionManager::Misuse is Exception {
    has $.message;
}
class X::Cro::ConnectionConditional::NoAlternatives is Exception {
    method message() {
        "Must construct Cro::ConnectionConditional with at least a default option"
    }
}
class X::Cro::ConnectionConditional::TransformOnly is Exception {
    has $.got;
    method message() {
        "Cro::ConnectionConditional options must be Cro::Transform (or an array of those), " ~
        "but got {$!got.^name}"
    }
}
class X::Cro::ConnectionConditional::NoDefault is Exception {
    method message() {
        "Must construct Cro::ConnectionConditional with a default option, not just conditions"
    }
}
class X::Cro::ConnectionConditional::MultipleDefaults is Exception {
    method message() {
        "Must not construct Cro::ConnectionConditional with multiple defaults"
    }
}
class X::Cro::ConnectionConditional::Incompatible is Exception {
    has $.consumes-a;
    has $.produces-a;
    has $.consumes-b;
    has $.produces-b;
    method message() {
        "Conditional pipelines must have same input and output messages types, but saw " ~
        "{$!consumes-a.^name} to {$!produces-a.^name} and " ~
        "{$!consumes-b.^name} to {$!produces-b.^name}"
    }
}

class Cro { ... }

class Cro::CompositeSource does Cro::Source {
    has @.components is required;

    method produces() { @!components.tail.produces }

    method incoming() returns Supply:D {
        my ($source, @rest) = @!components;
        my $current = $source.incoming;
        for @rest {
            $current = .transformer($current);
        }
        return $current;
    }
}

class Cro::CompositeTransform does Cro::Transform {
    has @.components is required;

    method consumes() { @!components.head.consumes }
    method produces() { @!components.tail.produces }

    method transformer(Supply:D $pipeline) returns Supply:D {
        my $current = $pipeline;
        for @!components {
            $current = .transformer($current);
        }
        return $current;
    }
}

class Cro::CompositeSink does Cro::Sink {
    has @.components is required;

    method consumes() { @!components.head.consumes }

    method sinker(Supply:D $pipeline) returns Supply:D {
        my @transforms = @!components;
        my $sink = @transforms.pop;
        my $current = $pipeline;
        for @transforms {
            $current = .transformer($current);
        }
        return $sink.sinker($current);
    }
}

class Cro::ConnectionConditional {
    has $.consumes;
    has $.produces;
    has @.conditions;
    has $.default;

    method new(**@options) {
        die X::Cro::ConnectionConditional::NoAlternatives.new unless @options;

        my $consumes;
        my $produces;
        sub check-compatibility($con, $prod) {
            state $first = True;
            if $first {
                $consumes = $con;
                $produces = $prod;
                $first = False;
            }
            else {
                if $con !=== $consumes || $prod !=== $produces {
                    die X::Cro::ConnectionConditional::Incompatible.new(
                        consumes-a => $consumes, produces-a => $produces,
                        consumes-b => $con, produces-b => $prod
                    );
                }
            }
        }

        my $saw-default = False;
        my @conditions;
        my $default;
        for @options {
            when Pair {
                check-compatibility(|check-and-get-endpoints(.value));
                push @conditions, $_;
            }
            default {
                if $saw-default {
                    die X::Cro::ConnectionConditional::MultipleDefaults.new;
                }
                $saw-default = True;
                check-compatibility(|check-and-get-endpoints($_));
                $default = $_;
            }
        }
        unless $saw-default {
            die X::Cro::ConnectionConditional::NoDefault.new;
        }

        self.bless(:$consumes, :$produces, :@conditions, :$default)
    }

    multi sub check-and-get-endpoints(Cro::Transform $t) {
        validate-transform($t);
        return ($t.consumes, $t.produces)
    }
    multi sub check-and-get-endpoints(@transforms) {
        my $expected;
        for @transforms.kv -> $i, $t {
            unless $t ~~ Cro::Transform {
                die X::Cro::ConnectionConditional::TransformOnly.new(got => $t);
            }
            validate-transform($t);
            if $i == 0 {
                $expected = $t.produces;
            }
            elsif $t.consumes !=== $expected {
                die X::Cro::Compose::Mismatch.new(
                    producer => @transforms[$i - 1],
                    consumer => $t
                );
            }
            else {
                $expected = $t.produces;
            }
        }
        return (@transforms[0].consumes, @transforms[*-1].produces);
    }
    multi sub check-and-get-endpoints($got) {
        die X::Cro::ConnectionConditional::TransformOnly.new(:$got)
    }

    sub validate-transform($t) {
        validate-consumer($t);
        validate-producer($t);
    }

    method consumes() { $!consumes }
    method produces() { $!produces }

    method select($data) {
        for @!conditions {
            return .value if .key()($data);
        }
        return $!default;
    }
}

role Cro::ConnectionState[::T] {
    method connection-state-type() { T }
}

class Cro::CompositeTransform::WithConnectionState is Cro::CompositeTransform {
    method transformer(Supply:D $pipeline) returns Supply:D {
        my $current = $pipeline;
        my %connection-state{Mu};
        for @.components -> $comp {
            if $comp ~~ Cro::ConnectionState {
                my $cs-type = $comp.connection-state-type;
                with %connection-state{$cs-type} {
                    $current = $comp.transformer($current, :connection-state($_));
                }
                else {
                    my $cs = $cs-type.new;
                    %connection-state{$cs-type} = $cs;
                    $current = $comp.transformer($current, :connection-state($cs));
                }
            }
            else {
                $current = $comp.transformer($current);
            }
        }
        return $current;
    }
}

class Cro::CompositeConnector does Cro::Connector {
    has @!before;
    has $!conditional-before;
    has $!connector;
    has @!after;
    has $!conditional-after;

    submethod BUILD(:@components! --> Nil) {
        my $seen-connector = False;
        for @components {
            when Cro::Connector {
                $!connector = $_;
                $seen-connector = True;
            }
            when $seen-connector {
                @!after.push($_);
                when Cro::ConnectionConditional {
                    $!conditional-after = True;
                }
            }
            default {
                @!before.push($_);
                when Cro::ConnectionConditional {
                    $!conditional-before = True;
                }
            }
        }
    }

    method consumes() {
        (@!before ?? @!before[0] !! $!connector).consumes
    }

    method produces() {
        (@!after ?? @!after[*-1] !! $!connector).produces
    }

    method connect(*%options --> Promise) {
        start {
            my Cro::Transform $con-tran = await $!connector.connect(|%options);
            my \before = $!conditional-before
                ?? filter(@!before, $con-tran)
                !! @!before;
            my \after = $!conditional-after
                ?? filter(@!after, $con-tran)
                !! @!after;
            Cro::CompositeTransform::WithConnectionState.new:
                components => flat(before, $con-tran, after)
        }
    }

    sub filter(@components, $con-tran) {
        @components.map: -> $c {
            $c ~~ Cro::ConnectionConditional
                ?? $c.select($con-tran).flat
                !! $c
        }
    }
}

class Cro::PipelineTraceTransform does Cro::Transform {
    has $.label;
    has $.component;
    has $.consumes;
    has $.produces;

    method transformer(Supply:D $in --> Supply) {
        supply {
            whenever $in -> \msg {
                my $output = (try msg.trace-output) // msg.perl;
                self!output-trace: "EMIT {encode $output}";
                emit msg;
                LAST {
                    self!output-trace: "DONE";
                }
                QUIT {
                    self!output-trace: "QUIT {encode .gist}";
                }
            }
        }
    }

    method !output-trace($message --> Nil) {
        note "[TRACE($!label)] $!component.^name() $message";
        try $*ERR.flush; # May throw on Windows, so guard with try
    }

    my $encode = ?%*ENV<CRO_TRACE_MACHINE_READABLE>;
    sub encode(Str $_) {
        $encode
            ?? .subst('\\', '\\\\', :g).subst("\n", '\\n', :g)
            !! $_
    }
}

class Cro::ConnectionManager does Cro::Sink {
    has Cro::Connection:U $!connection-type;
    has Cro::Transform $!transformer;
    has Cro::Sink $!sinker;

    submethod BUILD(:$!connection-type, :@components, :$debug, :$label) {
        if @components {
            my @debug = $debug
                ?? Cro::PipelineTraceTransform.new(
                    component => $!connection-type,
                    consumes => $!connection-type.produces,
                    produces => $!connection-type.produces,
                    :$label
                  )
                !! Empty;
            given Cro.compose(@debug, @components, :$debug, :$label, :for-connection) {
                when Cro::Sink {
                    if $!connection-type ~~ Cro::Replyable {
                        die X::Cro::ConnectionManager::Misuse.new: message =>
                            "A connection manager was inserted after " ~
                            "component {@components[0].^name}. The connection " ~
                            "type {$!connection-type.^name} is replyable, but " ~
                            "an explicit sink was also provided in the following " ~
                            "components"
                    }
                    else {
                        $!sinker = $_;
                    }
                }
                when Cro::Transform {
                    if $!connection-type ~~ Cro::Replyable {
                        $!transformer = $_;
                    }
                    else {
                        die X::Cro::ConnectionManager::Misuse.new: message =>
                            "A connection manager was inserted before component " ~
                            "{@components[0].^name}, but the connection was not " ~
                            "replyable and the remaining components did not finish " ~
                            "with a sink"
                    }
                }
                default {
                    die X::Cro::ConnectionManager::Misuse.new: message =>
                        "Components controlled by a connection manager must " ~
                        "compose to form a transform or a sink"
                }
            }
        }
        elsif $!connection-type !~~ Cro::Replyable {
            die X::Cro::ConnectionManager::Misuse.new: message =>
                "Can only create a connection manager from an empty list of " ~
                "components if the source connection type is replyable"
        }
    }

    method consumes() { $!connection-type }

    method sinker(Supply:D $incoming) {
        supply {
            whenever $incoming -> $connection {
                whenever self!start-connection($connection) {
                    QUIT {
                        default {
                            report-unhandled-error($_);
                        }
                    }
                }
            }
        }
    }

    method !start-connection(Cro::Connection $connection) {
        my $messages = $connection.incoming;
        my $to-sink = $!transformer
                ?? $!transformer.transformer($messages)
                !! $messages;
        return $!sinker
                ?? $!sinker.sinker($to-sink)
                !! $connection.replier.sinker($to-sink);
    }
}

class Cro {
    my $next-label-lock = Lock.new;
    my $next-label = 1;
    sub next-label() {
        $next-label-lock.protect: { $next-label++ }
    }

    my $debug-default = ?%*ENV<CRO_TRACE>;
    method compose(*@components-in, Cro::Service :$service-type, :$debug = $debug-default,
            :$label = "anon &next-label()", :$for-connection = False) {
        die X::Cro::Compose::Empty.new unless @components-in;

        # First scan through and see if we need to insert a connection
        # manager, which happens whenever there's a component that produces
        # connections, which are in turn consumed by something that wants to
        # get messages of the type the connection produces. We also do this if
        # the connection-producing thing comes last and is replyable.
        for flat @components-in Z @components-in[1..*] -> $comp, $next {
            ++state $split;
            if $comp.?produces ~~ Cro::Connection {
                if $comp.produces.produces === $next.?consumes {
                    return Cro.compose:
                        |@components-in[^$split],
                        Cro::ConnectionManager.new(
                            connection-type => $comp.produces,
                            components => @components-in[$split..*],
                            :$debug, :$label
                        ),
                        :$debug, :$label;
                }
            }
        }
        given @components-in[*-1] -> $comp {
            if $comp ~~ Cro::Source && $comp.produces ~~ Cro::Connection {
                if $comp.produces ~~ Cro::Replyable {
                    return Cro.compose:
                        |@components-in,
                        Cro::ConnectionManager.new(
                            connection-type => $comp.produces,
                            components => [],
                            :$debug, :$label
                        ),
                        :$debug, :$label;
                }
            }
        }

        my $has-source = False;
        my $has-sink = False;
        my $has-connector = False;
        my $has-connection-conditional = False;
        my $has-connection-state = False;
        my $expected;
        my @repliers-to-insert;
        my @components;

        sub check-and-add-replyable(Cro::Replyable $replyable) {
            my $replier = $replyable.replier;
            if $replier !~~ Cro::Transform && $replier !~~ Cro::Sink {
                die X::Cro::Compose::BadReplier.new(:$replyable);
            }
            @repliers-to-insert.unshift($replyable);
        }

        sub push-component($component) {
            push @components, $component;
            if $debug && $component !~~ Cro::Sink && $component !~~ Cro::PipelineTraceTransform {
                push @components, Cro::PipelineTraceTransform.new(
                    :$component, :$label,
                    consumes => $component.produces,
                    produces => $component.produces
                );
            }
        }

        for @components-in.kv -> $i, $comp {
            if @repliers-to-insert {
                my $replyable = @repliers-to-insert[0];
                my $replier = $replyable.replier;
                if $replier ~~ Cro::Transform && $replier.consumes === $expected {
                    shift @repliers-to-insert;
                    push-component $replier;
                    $expected = $replier.produces;
                }
            }

            given $comp {
                when Cro::Source {
                    unless $i == 0 {
                        die X::Cro::Compose::SourceMustBeFirst.new(source => $comp);
                    }
                    validate-producer($_);
                    $has-source = True;
                    when Cro::Replyable {
                        check-and-add-replyable($_);
                    }
                }
                when Cro::Transform {
                    validate-consumer($_);
                    validate-producer($_);
                    when Cro::Replyable {
                        check-and-add-replyable($_);
                    }
                    when Cro::ConnectionState {
                        $has-connection-state = True;
                    }
                }
                when Cro::ConnectionConditional {
                    validate-consumer($_);
                    validate-producer($_);
                    $has-connection-conditional = True;
                }
                when Cro::Connector {
                    die X::Cro::Compose::OnlyOneConnector.new if $has-connector;
                    validate-consumer($_);
                    validate-producer($_);
                    $has-connector = True;
                }
                when Cro::Sink {
                    unless $i == @components-in.end {
                        die X::Cro::Compose::SinkMustBeLast.new(sink => $comp);
                    }
                    validate-consumer($_);
                    $has-sink = True;
                }
                default {
                    die X::Cro::Compose::InvalidType.new(invalid => $comp);
                }
            }

            if $i == 0 {
                $expected = $comp.?produces;
            }
            elsif $comp.consumes !=== $expected {
                die X::Cro::Compose::Mismatch.new(
                    producer => @components[* - 1],
                    consumer => $comp
                );
            }
            else {
                $expected = $comp.?produces;
            }

            push-component $comp;
        }

        for @repliers-to-insert -> $replyable {
            given $replyable.replier {
                when Cro::Sink {
                    if $has-sink {
                        die X::Cro::Compose::TooManySinks.new(:$replyable);
                    }
                    if .consumes !=== $expected {
                        die X::Cro::Compose::Mismatch.new(
                            producer => @components[* - 1],
                            consumer => $_
                        );
                    }
                    push-component $_;
                    $has-sink = True;
                }
                when Cro::Transform {
                    if .consumes !=== $expected {
                        die X::Cro::Compose::Mismatch.new(
                            producer => @components[* - 1],
                            consumer => $_
                        );
                    }
                    push-component $_;
                    $expected = .produces;
                }
            }
        }

        if $has-connection-conditional && !$has-connector {
            die X::Cro::Compose::ConnectionConditionalWithoutConnector.new;
        }
        if $has-connection-state && !($has-connector || $for-connection && !$has-source && !$has-sink) {
            die X::Cro::Compose::ConnectionStateWithoutConnection.new;
        }

        if $has-connector {
            die X::Cro::Compose::SourceAndConnector.new if $has-source;
            die X::Cro::Compose::SinkAndConnector.new if $has-sink;
            Cro::CompositeConnector.new(:@components)
        }
        elsif $has-source && $has-sink {
            $service-type.bless(:@components)
        }
        elsif $has-source {
            Cro::CompositeSource.new(:@components)
        }
        elsif $has-sink {
            Cro::CompositeSink.new(:@components)
        }
        else {
            $has-connection-state
                ?? Cro::CompositeTransform::WithConnectionState.new(:@components)
                !! Cro::CompositeTransform.new(:@components)
        }
    }
}

my subset ConnectionOrMessage where Cro::Message | Cro::Connection;
sub validate-consumer($consumer) {
    unless $consumer.consumes ~~ ConnectionOrMessage {
        die X::Cro::Compose::BadConsumer.new(:$consumer);
    }
}
sub validate-producer($producer) {
    unless $producer.produces ~~ ConnectionOrMessage {
        die X::Cro::Compose::BadProducer.new(:$producer);
    }
}
