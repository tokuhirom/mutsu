use Issue7733::Conf;

class Issue7733::User is export {
    has Issue7733::Conf $.config;

    submethod BUILD(Issue7733::Conf :$config) {
        $!config = $config // Issue7733::Conf.new;
    }
}
