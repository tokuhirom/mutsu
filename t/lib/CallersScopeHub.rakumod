use CallersScopeOps;

class CallersScopeHub {
    method exports(@features --> Map()) {
        CallersScopeOps::EXPORT::ALL::,
    }
}

multi EXPORT(+@features) {
    CallersScopeHub.exports(@features)
}
