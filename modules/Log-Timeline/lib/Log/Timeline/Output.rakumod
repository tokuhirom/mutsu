#| Base role for a C<Log::Timeline> output, which is used to output the
#| logged events and tasks. If we are logging, then an instance of this
#| will be stored in C<< PROCESS::<$LOG-TIMELINE-OUTPUT> >>.
role Log::Timeline::Output {
    #| Logs an event.
    method log-event($type, Int $parent-id, Instant $timestamp, %data --> Nil) { ... }

    #| Logs the start of a task.
    method log-start($type, Int $parent-id, Int $id, Instant $timestamp, %data --> Nil) { ... }

    #| Logs the end of a task.
    method log-end($type, Int $id, Instant $timestamp --> Nil) { ... }

    #| Closes the output; typically happens at program shutdown.
    method close(--> Nil) { ... }
}

