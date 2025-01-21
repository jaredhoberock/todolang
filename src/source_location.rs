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

    pub fn advance_line(&mut self, source_char: char) {
        self.line += 1;
        self.column = 1;
        self.offset += source_char.len_utf8();
    }

    pub fn advance_column(&mut self, source_char: char) {
        self.column += 1;
        self.offset += source_char.len_utf8();
    }
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Line: {}, Column: {}", self.line, self.column)
    }
}

/// Represents a range within a source string, using an inclusive `start` location
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
pub struct SourceRange {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl SourceRange {
    pub fn line_of(source: &str, loc: &SourceLocation) -> SourceRange {
        // Cap offset within source bounds to avoid out-of-bounds access
        let offset = std::cmp::min(loc.offset, source.len());

        // Handle EOF on a trailing newline by moving back to the last content line
        let (effective_offset, effective_line) = if offset == source.len() && source.ends_with('\n')
        {
            (offset.saturating_sub(1), loc.line.saturating_sub(1))
        } else {
            (offset, loc.line)
        };

        // find the start of the line by searching backwards to the nearest newline
        let start_offset = source[..effective_offset]
            .rfind('\n')
            .map(|i| i + 1) // Move past the newline character
            .unwrap_or(0);

        // find the end of the line by locating the next newline or the end of the source
        let end_offset = source[start_offset..]
            .find('\n')
            .map(|i| start_offset + i) // Position end exclusive of newline character
            .unwrap_or(source.len()); // If no newline, go to the end of the source

        SourceRange {
            start: SourceLocation {
                line: effective_line,
                column: 1,
                offset: start_offset,
            },
            end: SourceLocation {
                line: effective_line,
                column: (end_offset - start_offset), // Exclusive length
                offset: end_offset,
            },
        }
    }

    pub fn line(&self) -> usize {
        self.start.line
    }

    /// Returns a `&str` for this range, with an exclusive end.
    pub fn as_str<'a>(&self, source: &'a str) -> &'a str {
        &source[self.start.offset..self.end.offset]
    }
}

impl std::fmt::Display for SourceRange {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Start: {}, End: {}", self.start, self.end)
    }
}
