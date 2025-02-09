use todolang::analysis::analyze_module;
use todolang::diagnostics::*;
use todolang::interpreter::Interpreter;
use todolang::lexer::Lexer;
use todolang::parser::parse_module;
use todolang::token::Token;

fn usage() {
    println!("usage: todolang script");
}

fn interpret(filename: &str, source: &str) -> Result<(), String> {
    let tokens: Vec<Token> = Lexer::new(&source).collect();
    let untyped_module = parse_module(&tokens)
        .map_err(|e| format_diagnostic_for_parse_error(&e, &filename, &source))?;
    let typed_module = analyze_module(&untyped_module)
        .map_err(|e| format_diagnostic_for_analysis_error(&e, &filename, &source))?;
    let mut interp = Interpreter::new();
    interp.interpret_module(&typed_module)
        .map_err(|e| format_diagnostic_for_interpreter_error(&e, &filename, &source))
}

fn interpret_from_file(filename: &str) -> Result<(),String> {
    let source = std::fs::read_to_string(filename)
        .map_err(|e| format!("Error reading '{}': {}", filename, e))?;
    interpret(filename, &source)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let result = if args.len() != 2 {
        usage();
        Ok(())
    } else {
        interpret_from_file(&args[1])
    };

    if let Err(e) = result {
        eprintln!("{}", e);
        std::process::exit(1)
    }
}
