use std::io::Write;

use todolang::interpreter::Interpreter;
use todolang::lexer::Lexer;
use todolang::parser::{parse_program, ParseError};
use todolang::source_location::SourceRange;
use todolang::token::{Token, TokenKind};

fn usage() {
    println!("usage: todolang [script]");
}

fn report_syntax_error(error: &ParseError, source: &str) -> () {
    eprintln!("Syntax error: {}", error);
    match &error.error_token {
        Some(token) => {
            let range = SourceRange::line_of(source, &token.location);
            let source_line = range.as_str(source);

            // print the line number and corresponding source line
            eprintln!("{:>4} | {}", range.line(), source_line);

            // if we have encountered EOF, point at one position past the end of the line
            let column = if token.kind == TokenKind::Eof {
                source_line.len()
            } else {
                token.location.column
            };

            eprintln!("     | {}^", " ".repeat(column));
        }
        _ => (),
    }
}

fn interpret(interp: &mut Interpreter, source: &str) -> bool {
    let tokens: Vec<Token> = Lexer::new(source).collect();

    let prog = match parse_program(&tokens) {
        Ok(prog) => prog,
        Err(error) => {
            report_syntax_error(&error, source);
            return false;
        }
    };

    match interp.interpret_program(&prog) {
        Ok(_) => true,
        Err(error) => {
            eprintln!("Runtime error: {}", error);
            false
        }
    }
}

fn interpret_from_file(filename: &str) -> bool {
    match std::fs::read_to_string(filename) {
        Ok(source) => {
            let mut interp = Interpreter::new();
            interpret(&mut interp, &source)
        }
        Err(error) => {
            eprintln!("Error reading file: {}", error);
            false
        }
    }
}

fn interpret_from_prompt() -> bool {
    let mut interp = Interpreter::new();
    let stdin = std::io::stdin();
    let mut input = String::new();

    loop {
        print!("> ");
        std::io::stdout().flush().unwrap();

        // clear previous input
        input.clear();

        match stdin.read_line(&mut input) {
            Ok(0) => return true, // EOF
            Ok(_) => interpret(&mut interp, &input),
            Err(error) => {
                eprintln!("Error reading line: {}", error);
                return false;
            }
        };
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() > 2 {
        usage();
        return;
    }

    let result = if args.len() == 2 {
        interpret_from_file(&args[1])
    } else {
        interpret_from_prompt()
    };

    std::process::exit(if result { 0 } else { -1 });
}
