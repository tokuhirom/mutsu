# A routine whose parameter shares its name with a package variable it reads
# through another package's name (the shape of User::Timezone's override).
my package State {
    my $tz;
    my $backup;
}

sub current is export { $State::tz // 'unset' }

package Z {
    our sub read-qualified($tz) { $State::tz }
}

my package EXPORT::override {
    &OUR::set-tz := sub set-tz(Str $tz) {
        $State::backup = $State::tz;
        $State::tz     = $tz;
    }
    &OUR::clear-tz := sub clear-tz {
        $State::tz = $State::backup;
    }
}
