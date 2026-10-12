# One reactor thread reads every async socket

Every tapped `IO::Socket::Async` connection used to own a `sock-conn` reader
thread that woke every 50 ms to notice when its taps had gone. A keep-alive
HTTP client or server (Cro, and LLM::Chat's fresh client per request) piled up
one polling thread per idle connection, and per-request latency grew with them
(#12865).

On Unix a single reactor thread now watches every connection's socket with
`poll(2)` and reads a socket only when it is readable, the way Rakudo's libuv
loop does. A new reader is handed over through a queue and a self-pipe wake-up.
Reads use `MSG_DONTWAIT` per call, so the socket's shared non-blocking flag
(which the writer's clone also sees) is never touched. A reader whose taps have
all gone is dropped without reading, leaving pending bytes in the kernel buffer
for the next `.Supply` tap as before. Other platforms keep the per-connection
thread.

Each tap's consumer side still holds a pooled worker; that is #12892.
