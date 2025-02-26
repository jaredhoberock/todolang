use std::cmp::Ordering;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
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

impl PartialOrd for SourceLocation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.offset.partial_cmp(&other.offset)
    }
}

impl Ord for SourceLocation {
    fn cmp(&self, other: &Self) -> Ordering {
        self.offset.cmp(&other.offset)
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

    pub fn merge(&self, other: &SourceSpan) -> SourceSpan {
        let start = std::cmp::min(&self.start, &other.start).clone();
        let end = std::cmp::max(&self.end, &other.end).clone();
        SourceSpan::new(start, end)
    }
}

impl std::fmt::Display for SourceSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Start: {}, End: {}", self.start, self.end)
    }
}

impl From<SourceSpan> for miette::SourceSpan {
    fn from(span: SourceSpan) -> miette::SourceSpan {
        miette::SourceSpan::new(
            span.start.offset.into(), 
            (span.end.offset - span.start.offset).into()
        )
    }
}

pub trait Locatable {
    fn location(&self) -> SourceSpan;
}

impl<T: Locatable> Locatable for Box<T> {
    fn location(&self) -> SourceSpan {
        (**self).location()
    }
}
