use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::Buffer;
use codespan_reporting::term::{emit, Config};
use crate::analysis::Error as AnalysisError;
use crate::analysis::TypeMismatchError;

fn add_labels_for_type_mismatch_error(mismatch: &TypeMismatchError, diagnostic:&mut Diagnostic<()>) {
    diagnostic.labels.push(
        Label::secondary((), mismatch.expected.1.as_range())
        .with_message(format!("expected type '{}' found here", mismatch.expected.0))
    );
    diagnostic.labels.push(
        Label::secondary((), mismatch.found.1.as_range())
        .with_message(format!("expected '{}', found '{}'", mismatch.expected.0, mismatch.found.0))
    );
}

pub fn format_diagnostic_for_analysis_error(error: &AnalysisError, filename: &str, source: &str) -> String {
    let file = SimpleFile::new(filename, source);

    let mut diagnostic = Diagnostic::error().with_message(error.to_string());

    diagnostic.labels.push(Label::primary((), error.location().as_range()));

    // add additional context
    match error {
        AnalysisError::TypeMismatch(mismatch) => {
            add_labels_for_type_mismatch_error(&mismatch, &mut diagnostic);
        },
        _ => (),
    }

    let mut writer = Buffer::ansi();
    emit(&mut writer, &Config::default(), &file, &diagnostic)
        .expect("failed to write diagnostic");

    String::from_utf8(writer.into_inner())
        .expect("diagnostic output was not valid UTF-8")
}
