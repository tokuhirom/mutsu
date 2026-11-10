# Stop tracking the io-out-buffer scratch files

Three `tmp-io-out-buffer-<pid>.txt` scratch files had been committed at the repository root.
They are removed and ignored, and `t/io/io-out-buffer.t` now writes its scratch file under
`$*TMPDIR` instead of the working directory.
