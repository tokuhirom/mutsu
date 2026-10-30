use LateImportProtoGreet::Inner;

class LateImportProtoOuter {
    method exports(--> Map()) {
        LateImportProtoGreet::Inner::EXPORT::ALL::,
        '&late-proto' => &late-proto,
    }
}

multi EXPORT(+@features) {
    LateImportProtoOuter.exports
}
