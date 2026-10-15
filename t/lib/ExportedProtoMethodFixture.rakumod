unit module ExportedProtoMethodFixture;

class ExpM {
    proto method load-it($?: |c) is export(:load-it) {*}
    multi method load-it($class = $?CLASS: Int :$a!, |c) { "a:$a" }
}
