use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::Buffer;
use codespan_reporting::term::{emit, Config};
use crate::analysis::Error as AnalysisError;

pub fn format_diagnostic_for_analysis_error(error: &AnalysisError, filename: &str, source: &str) -> String {
    let file = SimpleFile::new(filename, source);

    let mut diagnostic = Diagnostic::error().with_message(error.to_string());

    diagnostic.labels.push(Label::primary((), error.location().as_range()));

    // add additional context to type mismatches
    match error {
        AnalysisError::Mismatch{ expected_at, .. } => {
            diagnostic.labels.push(
                Label::secondary((), expected_at.as_range())
                .with_message("expected type found here")
            );
        },
        _ => (),
    }

    let mut writer = Buffer::ansi();
    emit(&mut writer, &Config::default(), &file, &diagnostic)
        .expect("failed to write diagnostic");

    String::from_utf8(writer.into_inner())
        .expect("diagnostic output was not valid UTF-8")
}
