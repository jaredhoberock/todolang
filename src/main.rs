use std::io::Write;

use todolang::analysis::analyze_module;
use todolang::diagnostics::*;
use todolang::interpreter::Interpreter;
use todolang::interpreter2::Interpreter as Interpreter2;
use todolang::lexer::Lexer;
use todolang::parser::{parse_global_statement_or_eof, parse_program};
use todolang::parser2::parse_module;
use todolang::syntax::Program;
use todolang::token::Token;

fn usage() {
    println!("usage: todolang [script]");
}

fn interpret2(filename: &str, source: &str) -> Result<(), String> {
    let tokens: Vec<Token> = Lexer::new(&source).collect();
    let untyped_module = parse_module(&tokens)
        .map_err(|e| format_diagnostic_for_parse_error2(&e, &filename, &source))?;
    let typed_module = analyze_module(&untyped_module)
        .map_err(|e| format_diagnostic_for_analysis_error(&e, &filename, &source))?;
    let mut interp = Interpreter2::new();
    interp.interpret_module(&typed_module)
        .map_err(|e| format_diagnostic_for_interpreter_error2(&e, &filename, &source))
}

fn interpret_from_file2(filename: &str) -> Result<(),String> {
    let source = std::fs::read_to_string(filename)
        .map_err(|e| format!("Error reading '{}': {}", filename, e))?;
    interpret2(filename, &source)
}

fn interpret(filename: &str, source: &str) -> Result<(),String> {
    let tokens: Vec<Token> = Lexer::new(&source).collect();
    let prog = parse_program(&tokens).map_err(|e| format_diagnostic_for_parse_error(&e, &filename, &source))?;
    let mut interp = Interpreter::new();
    interp.interpret_program(&prog).map_err(|e| format_diagnostic_for_interpreter_error(&e, &filename, &source))
}

fn interpret_from_file(filename: &str) -> Result<(),String> {
    let source = std::fs::read_to_string(filename)
        .map_err(|e| format!("Error reading file: {}", e))?;
    interpret(filename, &source)
}

fn evaluate_global_statement(interp: &mut Interpreter, prog: &mut Program, source: &str) -> Result<(),String> {
    let tokens: Vec<Token> = Lexer::new(source).collect();

    // try to parse the next statement
    let stmt = match parse_global_statement_or_eof(&tokens) {
        Ok(None) => { return Ok(()) }, // EOF
        Ok(Some(stmt)) => { 
            prog.statements.push(Box::new(stmt));
            Ok(prog.statements.last().unwrap())
        },
        Err(error) => Err(format_diagnostic_for_parse_error(&error, "<stdin>", source)),
    }?;

    interp.interpret_global_statement(stmt)
        .map_err(|e| format_diagnostic_for_interpreter_error(&e, "<stdin>", &source))
}

fn interpret_from_prompt() -> Result<(),String> {
    let mut prog = Program::new();
    let mut interp = Interpreter::new();
    let mut input = String::new();

    let stdin = std::io::stdin();
    
    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        // clear previous input
        input.clear();

        let n = stdin.read_line(&mut input)
            .map_err(|e| format!("Error reading line: {}", e))?;

        if n == 0 {
            // EOF
            return Ok(());
        }

        evaluate_global_statement(&mut interp, &mut prog, &input)?;
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let result = if args.len() > 3 {
        usage();
        Ok(())
    } else if args.len() == 3 {
        interpret_from_file2(&args[1])
    } else if args.len() == 2 {
        interpret_from_file(&args[1])
    } else {
        interpret_from_prompt()
    };

    if let Err(e) = result {
        eprintln!("{}", e);
        std::process::exit(1)
    }
}
