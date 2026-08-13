class X::Cro::Service::StopWithoutStart is Exception {
    method message {
        "The service was not started and the stop method is called on it, a missing .start?"
    }
}

role Cro::Service {
    has @.components is required;
    has Tap $!service-tap;

    method start(--> Nil) {
        $!service-tap = self!assemble-pipeline().tap: quit => { .note };
    }

    method !assemble-pipeline() {
        my @transforms = @!components;
        my $source = @transforms.shift;
        my $sink = @transforms.pop;
        my $current = $source.incoming;
        for @transforms {
            $current = .transformer($current);
        }
        return $sink.sinker($current);
    }

    method stop(--> Nil) {
        with $!service-tap {
            .close;
            $!service-tap = Nil;
        } else {
            die X::Cro::Service::StopWithoutStart.new;
        }
    }
}
