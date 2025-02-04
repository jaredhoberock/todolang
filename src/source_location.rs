#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
    pub offset: usize, // byte offset into source
}

impl SourceLocation {
    pub fn new() -> Self {
        SourceLocation {
            line: 1,
            column: 1,
            offset: 0,
        }
    }

    pub fn advance(&mut self, source_char: char) {
        self.offset += source_char.len_utf8();
        if source_char == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Line: {}, Column: {}", self.line, self.column)
    }
}

/// Represents a span within a source string, using an inclusive `start` location
/// and an exclusive `end` location.
///
/// The `start` field marks the beginning of the range and is an inclusive, physical
/// offset within the source. The `end` field marks the end of the range and is an
/// exclusive, logical offset. This means:
///
/// - `start` points to the actual offset in `source` where the range begins.
/// - `end` is exclusive, marking the end of the range one position past the last character,
///   which may be one character beyond `source` if the range extends to `EOF`.
///
/// This design aligns with Rust's exclusive slice semantics, making it easy to obtain
/// a substring using `source[start.offset..end.offset]`.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceSpan {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl SourceSpan {
    pub fn new(start: SourceLocation, end: SourceLocation) -> Self {
        Self { start, end }
    }

    pub fn line(&self) -> usize {
        self.start.line
    }

    /// Returns a `&str` for this range, with an exclusive end.
    pub fn as_str<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start.offset..self.end.offset]
    }

    pub fn as_range(&self) -> std::ops::Range<usize> {
        self.start.offset..self.end.offset
    }
}

impl std::fmt::Display for SourceSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Start: {}, End: {}", self.start, self.end)
    }
}

pub trait Locatable {
    fn source_span(&self) -> SourceSpan;
}

impl<T: Locatable> Locatable for Box<T> {
    fn source_span(&self) -> SourceSpan {
        (**self).source_span()
    }
}
