# Loading this compunit leaves a mark that is visible before any method of the
# module that `use`s it has run.
%*ENV<MUTSU_METHOD_BODY_USE_LATE> = 'loaded';

class MethodBodyUseLate::Marker {
    method tag { 'late-marker' }
}
