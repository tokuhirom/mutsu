//! A read buffer for the private handle `IO::Path.lines` / `.words` open.
//!
//! `IO::Handle`'s ordinary file reads go straight to the unbuffered `fs::File`,
//! one byte per `read(2)`, because a user handle can `.seek`/`.tell`/`.write`
//! between reads and must see the kernel's file offset. The handle behind
//! `$path.IO.lines` is never handed to the user (Rakudo's
//! `IO::Path.lines` is `self.open(...).lines(:close)`), so nothing can observe
//! its offset and its reads can be buffered. Without the buffer a full read of
//! the file would cost one syscall per byte (#9257).
use std::fs;
use std::io::Read;

const CAPACITY: usize = 64 * 1024;

#[derive(Debug)]
pub(crate) struct SeqFileReader {
    file: fs::File,
    buf: Vec<u8>,
    pos: usize,
    /// Drop a UTF-8 BOM at the start of the file (a UTF-8 text read) --
    /// before the first record is cut, so a BOM-only file has no lines.
    skip_bom: bool,
}

impl SeqFileReader {
    pub(crate) fn new(file: fs::File, skip_utf8_bom: bool) -> Self {
        Self {
            file,
            buf: Vec::new(),
            pos: 0,
            skip_bom: skip_utf8_bom,
        }
    }

    /// A copy for a thread's handle table (`runtime_thread.rs`). Like the
    /// `fs::File::try_clone` the other handles use there, the two copies share
    /// the kernel file offset; the bytes already buffered are copied.
    pub(crate) fn try_clone(&self) -> Option<Self> {
        Some(Self {
            file: self.file.try_clone().ok()?,
            buf: self.buf.clone(),
            pos: self.pos,
            skip_bom: self.skip_bom,
        })
    }
}

impl SeqFileReader {
    /// The next record, cut at the longest separator that ends it -- the same
    /// answer `Interpreter::read_record_bytes` gives, but scanning the buffer
    /// in place instead of one `read` call and one separator test per byte.
    /// A separator can only end at a byte equal to its last byte, so only
    /// those bytes are tested. `None` at EOF.
    // Cost: O(r * s), r = bytes of the record, s = separators ending in the
    // same byte as it.
    pub(crate) fn read_record(
        &mut self,
        separators: &[Vec<u8>],
        chomp: bool,
    ) -> std::io::Result<Option<Vec<u8>>> {
        let mut record = Vec::new();
        let mut read_any = false;
        loop {
            if self.pos >= self.buf.len() && !self.refill()? {
                break;
            }
            read_any = true;
            let chunk = &self.buf[self.pos..];
            let mut copied = 0;
            for (i, &byte) in chunk.iter().enumerate() {
                if !separators.iter().any(|sep| sep.last() == Some(&byte)) {
                    continue;
                }
                record.extend_from_slice(&chunk[copied..=i]);
                copied = i + 1;
                if let Some(matched_len) = separators
                    .iter()
                    .filter(|sep| record.ends_with(sep))
                    .map(|sep| sep.len())
                    .max()
                {
                    self.pos += copied;
                    if chomp {
                        record.truncate(record.len() - matched_len);
                    }
                    return Ok(Some(record));
                }
            }
            record.extend_from_slice(&chunk[copied..]);
            self.pos = self.buf.len();
        }
        Ok(read_any.then_some(record))
    }

    /// Read the next block into the buffer; `false` at EOF.
    fn refill(&mut self) -> std::io::Result<bool> {
        self.buf.resize(CAPACITY, 0);
        let mut n = self.file.read(&mut self.buf)?;
        if self.skip_bom {
            self.skip_bom = false;
            // A read may return fewer than 3 bytes before EOF; top up so a BOM
            // split across reads is still recognized.
            while n > 0 && n < 3 {
                let more = self.file.read(&mut self.buf[n..])?;
                if more == 0 {
                    break;
                }
                n += more;
            }
            if self.buf[..n].starts_with(b"\xEF\xBB\xBF") {
                self.buf.copy_within(3..n, 0);
                n -= 3;
                if n == 0 {
                    return self.refill();
                }
            }
        }
        self.buf.truncate(n);
        self.pos = 0;
        Ok(n > 0)
    }
}

impl Read for SeqFileReader {
    // Cost: O(out.len()) amortized; one read(2) per CAPACITY bytes.
    fn read(&mut self, out: &mut [u8]) -> std::io::Result<usize> {
        if self.pos >= self.buf.len() && !self.refill()? {
            return Ok(0);
        }
        let n = out.len().min(self.buf.len() - self.pos);
        out[..n].copy_from_slice(&self.buf[self.pos..self.pos + n]);
        self.pos += n;
        Ok(n)
    }
}
