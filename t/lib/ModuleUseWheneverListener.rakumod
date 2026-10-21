unit module ModuleUseWheneverListener;
use ModuleUseWheneverHelper;
our sub incoming(Supply $connections) {
    supply {
        whenever $connections -> $socket {
            emit note-conn($socket);
        }
    }
}
