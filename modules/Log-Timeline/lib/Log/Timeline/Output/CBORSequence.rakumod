use Log::Timeline::Output;
use CBOR::Simple;

#| Outputs the log into a file as a CBOR Sequence (IETF RFC 8742),
#| one encoded CBOR value per logged event.
class Log::Timeline::Output::CBORSequence does Log::Timeline::Output {
    #| The file to log into.
    has IO::Path $.path is required;

    #| The handle to write to.
    has IO::Handle $!handle = open($!path, :w);

    #| Logs an event.
    method log-event($type, Int $parent-id, Instant $timestamp, %data --> Nil) {
        $!handle.write: cbor-encode {
            :m($type.module), :c($type.category), :n($type.name), :k(0),
            :p($parent-id), :t($timestamp), :d(%data)
        }
    }

    #| Logs the start of a task.
    method log-start($type, Int $parent-id, Int $id, Instant $timestamp, %data --> Nil) {
        $!handle.write: cbor-encode {
            :m($type.module), :c($type.category), :n($type.name), :k(1),
            :i($id), :p($parent-id), :t($timestamp), :d(%data)
        }
    }

    #| Logs the end of a task.
    method log-end($type, Int $id, Instant $timestamp --> Nil) {
        $!handle.write: cbor-encode {
            :m($type.module), :c($type.category), :n($type.name), :k(2),
            :i($id), :t($timestamp)
        }
    }

    #| Close the output file handle, flushing any events.
    method close(--> Nil) {
        $!handle.close;
    }
}
