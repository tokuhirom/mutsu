use CatchInlineExport;
unit class CatchInlineUser;

# The CATCH handler runs inline at the throw site, which is in the caller's
# compunit; its imported routine must still resolve in this unit.
method guarded(&thrower) {
    try {
        thrower();
        CATCH { default { return wrap-text(.message, :error) } }
    }
}

# `return` from the handler of a `try` inside an anonymous sub targets that sub.
method guarded-anon(&thrower) {
    my &run = sub {
        try {
            thrower();
            CATCH { default { return wrap-text(.message.uc, :error) } }
        }
    };
    run();
}
