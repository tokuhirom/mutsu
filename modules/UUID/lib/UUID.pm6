class UUID {
    has $.bytes;
    has $.version;

    method new(:$version = 4) {
        if $version == 4 {
            my @bytes = (0..255).roll(16);

            #variant
            @bytes[8] +|= 0b10000000;
            @bytes[8] +&= 0b10111111;

            #version
            @bytes[6] +|= 0b01000000;
            @bytes[6] +&= 0b01001111;

            self.bless(:bytes(buf8.new(@bytes)), :$version);
        }
        else {
            die "UUID version $version not supported.";
        }
    }

    method Str {
        (:256[$.bytes.values].fmt("%32.32x")
            ~~ /(........)(....)(....)(....)(............)/)
            .join("-");
    }

    method Blob {
        $.bytes;
    }
}
