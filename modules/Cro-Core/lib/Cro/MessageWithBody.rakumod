use Cro::Message;

class X::Cro::BodyNotText is Exception {
    has $.message;
    method message() { "$!message.^name() does not have a text body" }
}

role Cro::MessageWithBody does Cro::Message {
    has Supply $!body-byte-stream; # Typically set when receiving from network
    has $!body;                    # Typically set when producing locally

    method set-body-byte-stream(Supply $!body-byte-stream --> Nil) {
        $!body = Nil;
    }

    method set-body($!body --> Nil) {
        $!body-byte-stream = Nil;
    }

    method has-body() {
        $!body.DEFINITE || $!body-byte-stream.DEFINITE
    }

    method body-byte-stream(--> Supply) {
        with $!body-byte-stream {
            $_
        }
        orwith $!body {
            self.body-serializer-selector.select(self, $_).serialize(self, $_)
        }
        else {
            supply { }
        }
    }

    method body-blob(--> Promise) {
        Promise(supply {
            my $joined = Buf.new;
            whenever self.body-byte-stream -> $blob {
                $joined.append($blob);
                LAST emit $joined;
            }
        })
    }

    method body-text(--> Promise) {
        self.body-blob.then: -> $blob-promise {
            my $blob = $blob-promise.result;
            given self.body-text-encoding($blob) {
                when Str {
                    $blob.decode($_)
                }
                when Iterable {
                    my $result;
                    my $error;
                    for @$_ -> $try-enc {
                        $result = $blob.decode($try-enc);
                        CATCH {
                            default {
                                $error = $_;
                            }
                        }
                    }
                    $result // $error.rethrow
                }
                default {
                    die X::Cro::BodyNotText.new(message => self);
                }
            }
        }
    }

    method body-text-encoding(Blob $body) {
        die X::Cro::BodyNotText.new(message => self);
    }

    method body(--> Promise) {
        self.body-parser-selector.select(self).parse(self)
    }

    method body-parser-selector() { ... }
    method body-serializer-selector() { ... }

    method error-hint() { Nil }
}
