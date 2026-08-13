sub set-unhandled-error-reporter(&reporter) is export {
    PROCESS::<$CRO-UNHANDLED-ERROR-REPORTER> = &reporter;
}
sub report-unhandled-error($error) is export {
    PROCESS::<$CRO-UNHANDLED-ERROR-REPORTER>($error)
}
set-unhandled-error-reporter({ .note })
