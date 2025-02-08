use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::Buffer;
use codespan_reporting::term::{emit, Config};

use crate::parser::ParseError;
use crate::interpreter::Error as InterpreterError;
use crate::token::TokenKind;

pub fn format_diagnostic_for_parse_error(error: &ParseError, filename: &str, source: &str) -> String {
    let file = SimpleFile::new(filename, source);

    let mut diagnostic = Diagnostic::error().with_message(error.to_string());

    if let Some(token) = &error.error_token {
        let range = if token.kind == TokenKind::Eof {
            let trimmed = source.trim_end_matches('\n');
            let offset = trimmed.len();
            offset..offset
        } else {
            token.location.as_range()
        };

        let label_message = if token.kind == TokenKind::Eof {
            "unexpected end of input"
        } else {
            "unexpected token"
        };

        diagnostic.labels.push(
            Label::primary((), range)
                .with_message(label_message)
        );
    }

    let mut writer = Buffer::ansi();
    emit(&mut writer, &Config::default(), &file, &diagnostic)
        .expect("failed to write diagnostic");

    String::from_utf8(writer.into_inner())
        .expect("diagnostic output was not valid UTF-8")
}

pub fn format_diagnostic_for_interpreter_error(error: &InterpreterError, filename: &str, source: &str) -> String {
    let file = SimpleFile::new(filename, source);

    let mut diagnostic = Diagnostic::error().with_message(error.to_string());
    diagnostic.labels.push(Label::primary((), error.location.as_range()));

    let mut writer = Buffer::ansi();
    emit(&mut writer, &Config::default(), &file, &diagnostic)
        .expect("failed to write diagnostic");

    String::from_utf8(writer.into_inner())
        .expect("diagnostic output was not valid UTF-8")
}
