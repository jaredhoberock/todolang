use codespan_reporting::diagnostic::Label;
use codespan_reporting::diagnostic::Diagnostic as CsDiagnostic;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::Buffer;
use codespan_reporting::term::{emit, Config};
use crate::analysis::Error as AnalysisError;
use miette::Diagnostic as MietteDiagnostic;
use miette::*;

// this renders a miette diagnostic natively
// it looks nicer in some ways, worse in some ways
fn _format_miette_diagnostic(error: AnalysisError, filename: &str, source: &str) -> String {
    let mut theme = GraphicalTheme::unicode();
    theme.characters.error = "error:".to_string();

    let named_source = NamedSource::new(filename.to_owned(), source.to_owned());
    let report = Report::new(error)
        .with_source_code(named_source);
    let mut output = String::new();
    GraphicalReportHandler::new_themed(theme)
        .render_report(&mut output, &*report)
        .expect("Failed to render report");
    output
}

fn location_of_miette_diagnostic(diag: &impl MietteDiagnostic) -> Option<std::ops::Range<usize>> {
    let mut first_secondary = None;
    if let Some(labels) = diag.labels() {
        for label in labels {
            // Save the first label as a fallback
            if first_secondary.is_none() {
                let start = label.offset();
                let end = start + label.len();
                first_secondary = Some(start..end);
            }

            // If this label is primary, return its span immediately
            if label.primary() {
                let start = label.offset();
                let end = start + label.len();
                return Some(start..end);
            }
        }
    }
    first_secondary
}

fn to_codespan_diagnostic(error: AnalysisError) -> CsDiagnostic<()> {
    let mut diag = CsDiagnostic::<()>::error()
        .with_message(error.to_string());
    
    // find the overall location of the error, if it exists
    if let Some(loc) = location_of_miette_diagnostic(&error) {
        diag.labels.push(Label::primary((), loc));
    }

    // render all labels as secondary labels
    if let Some(labels) = error.labels() {
        for label in labels {
            let start = label.offset();
            let end = start + label.len();
            let loc = start..end;
            let msg = label.label().unwrap_or("");
            diag.labels.push(Label::secondary((), loc.clone()).with_message(msg));
        }
    }

    diag
}

pub fn format_diagnostic_for_analysis_error(error: AnalysisError, filename: &str, source: &str) -> String {
    let file = SimpleFile::new(filename, source);

    let diagnostic = to_codespan_diagnostic(error);

    let mut writer = Buffer::ansi();
    emit(&mut writer, &Config::default(), &file, &diagnostic)
        .expect("failed to write diagnostic");

    String::from_utf8(writer.into_inner())
        .expect("diagnostic output was not valid UTF-8")
}
