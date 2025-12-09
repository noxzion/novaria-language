use crate::lexer::Token;
use crate::ast::*;

pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
    file: String,
}

impl Parser {
    pub fn new(tokens: Vec<Token>, file: &str) -> Self {
        Parser {
            tokens,
            position: 0,
            file: file.to_string(),
        }
    }

    fn current_token(&self) -> &Token {
        if self.position < self.tokens.len() {
            &self.tokens[self.position]
        } else {
            &Token::Eof
        }
    }

    fn advance(&mut self) {
        self.position += 1;
    }

    fn skip_newlines(&mut self) {
        while matches!(self.current_token(), Token::Newline) {
            self.advance();
        }
    }

    fn expect(&mut self, expected: Token) -> crate::error::Result<()> {
        if self.current_token() != &expected {
            return Err(crate::error::CompileError::new(
                crate::error::ErrorKind::ParserError,
                format!("expected {:?}, found {:?}", expected, self.current_token()),
                self.file.clone(),
                1,
                1,
            ));
        }
        self.advance();
        Ok(())
    }

    fn error(&self, message: String) -> crate::error::CompileError {
        crate::error::CompileError::new(
            crate::error::ErrorKind::ParserError,
            message,
            self.file.clone(),
            1,
            1,
        )
    }

    pub fn parse(&mut self) -> crate::error::Result<Program> {
        self.skip_newlines();

        self.expect(Token::Package)?;
        let package = if let Token::Identifier(name) = self.current_token() {
            let p = name.clone();
            self.advance();
            p
        } else {
            return Err(self.error("expected package name".to_string()));
        };

        self.skip_newlines();

        let mut imports = Vec::new();
        while matches!(self.current_token(), Token::Import) {
            self.advance();
            if let Token::String(path) = self.current_token() {
                let import = Import {
                    path: path.clone(),
                    alias: None,
                };
                imports.push(import);
                self.advance();
            } else {
                return Err(self.error("expected import path string".to_string()));
            }
            self.skip_newlines();
        }

        let mut functions = Vec::new();
        while !matches!(self.current_token(), Token::Eof) {
            self.skip_newlines();
            if matches!(self.current_token(), Token::Eof) {
                break;
            }
            functions.push(self.parse_function()?);
        }

        Ok(Program {
            package,
            imports,
            functions,
            modules: std::collections::HashMap::new(),
        })
    }

    fn parse_function(&mut self) -> crate::error::Result<Function> {
        let is_pub = if let Token::Identifier(id) = self.current_token() {
            if id == "pub" {
                self.advance();
                true
            } else {
                false
            }
        } else {
            false
        };

        self.expect(Token::Func)?;

        let name = if let Token::Identifier(n) = self.current_token() {
            let name = n.clone();
            self.advance();
            name
        } else {
            return Err(self.error("expected function name".to_string()));
        };

        self.expect(Token::LeftParen)?;
        let mut params = Vec::new();

        while !matches!(self.current_token(), Token::RightParen) {
            let param_name = if let Token::Identifier(n) = self.current_token() {
                let name = n.clone();
                self.advance();
                name
            } else {
                return Err(self.error("expected parameter name".to_string()));
            };

            if matches!(self.current_token(), Token::Colon) {
                self.advance();
            }

            let param_type = if let Token::Identifier(t) = self.current_token() {
                let ty = t.clone();
                self.advance();
                ty
            } else {
                return Err(self.error("expected parameter type".to_string()));
            };

            params.push(Parameter {
                name: param_name,
                param_type,
            });

            if matches!(self.current_token(), Token::Comma) {
                self.advance();
            }
        }

        self.expect(Token::RightParen)?;

        let return_type = if matches!(self.current_token(), Token::Arrow) {
            self.advance();
            if let Token::Identifier(t) = self.current_token() {
                let ty = t.clone();
                self.advance();
                Some(ty)
            } else {
                None
            }
        } else if let Token::Identifier(t) = self.current_token() {
            let ty = t.clone();
            self.advance();
            Some(ty)
        } else {
            None
        };

        self.skip_newlines();
        self.expect(Token::LeftBrace)?;
        self.skip_newlines();

        let mut body = Vec::new();
        while !matches!(self.current_token(), Token::RightBrace) {
            body.push(self.parse_statement()?);
            self.skip_newlines();
        }

        self.expect(Token::RightBrace)?;
        self.skip_newlines();

        let is_exported = is_pub || name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false);

        Ok(Function {
            name,
            params,
            return_type,
            body,
            is_exported,
        })
    }

    fn parse_statement(&mut self) -> crate::error::Result<Statement> {
        match self.current_token() {
            Token::Var => self.parse_var_decl(),
            Token::If => self.parse_if(),
            Token::For => self.parse_for(),
            Token::Return => self.parse_return(),
            Token::Asm => self.parse_asm(),
            Token::Star => {
                // Check if this is a pointer assignment (*ptr = value)
                let next_pos = self.position + 1;
                let mut check_pos = next_pos;
                // Skip identifier tokens to find assignment
                while check_pos < self.tokens.len() {
                    match &self.tokens[check_pos] {
                        Token::Assign => {
                            return self.parse_pointer_assignment();
                        }
                        Token::Identifier(_) | Token::LeftParen | Token::RightParen => {
                            check_pos += 1;
                        }
                        _ => break,
                    }
                }
                Ok(Statement::Expression(self.parse_expression()))
            }
            Token::Identifier(_) => {
                let next_pos = self.position + 1;
                if next_pos < self.tokens.len() && (matches!(self.tokens[next_pos], Token::Assign) || matches!(self.tokens[next_pos], Token::LBracket)) {
                    self.parse_assignment()
                } else {
                    Ok(Statement::Expression(self.parse_expression()))
                }
            }
            _ => Ok(Statement::Expression(self.parse_expression())),
        }
    }

    fn parse_var_decl(&mut self) -> crate::error::Result<Statement> {
        self.expect(Token::Var)?;

        let name = if let Token::Identifier(n) = self.current_token() {
            let name = n.clone();
            self.advance();
            name
        } else {
            return Err(self.error("expected variable name".to_string()));
        };

        if matches!(self.current_token(), Token::Colon) {
            self.advance();
        }

        if matches!(self.current_token(), Token::LBracket) {
            self.advance();

            let size = if let Token::Number(n) = self.current_token() {
                let s = *n as usize;
                self.advance();
                s
            } else {
                return Err(self.error("expected array size".to_string()));
            };

            self.expect(Token::RBracket)?;

            let element_type = if let Token::Identifier(t) = self.current_token() {
                let ty = t.clone();
                self.advance();
                ty
            } else {
                return Err(self.error("expected array element type".to_string()));
            };

            return Ok(Statement::ArrayDecl { name, element_type, size });
        }

        let var_type = if let Token::Identifier(t) = self.current_token() {
            let ty = t.clone();
            self.advance();
            Some(ty)
        } else {
            None
        };

        let value = if matches!(self.current_token(), Token::Assign) {
            self.advance();
            Some(self.parse_expression())
        } else {
            None
        };

        Ok(Statement::VarDecl { name, var_type, value })
    }

    fn parse_assignment(&mut self) -> crate::error::Result<Statement> {
        let name = if let Token::Identifier(n) = self.current_token() {
            let name = n.clone();
            self.advance();
            name
        } else {
            return Err(self.error("expected variable name".to_string()));
        };

        if matches!(self.current_token(), Token::LBracket) {
            self.advance();
            let index = self.parse_expression();
            self.expect(Token::RBracket)?;
            self.expect(Token::Assign)?;
            let value = self.parse_expression();

            return Ok(Statement::ArrayAssignment { name, index, value });
        }

        self.expect(Token::Assign)?;
        let value = self.parse_expression();

        Ok(Statement::Assignment { name, value })
    }

    fn parse_pointer_assignment(&mut self) -> crate::error::Result<Statement> {
        // Parse *ptr = value
        self.expect(Token::Star)?;
        let target = self.parse_primary();
        self.expect(Token::Assign)?;
        let value = self.parse_expression();

        Ok(Statement::PointerAssignment { target, value })
    }

    fn parse_if(&mut self) -> crate::error::Result<Statement> {
        self.expect(Token::If)?;

        let condition = self.parse_expression();

        self.skip_newlines();
        self.expect(Token::LeftBrace)?;
        self.skip_newlines();

        let mut then_body = Vec::new();
        while !matches!(self.current_token(), Token::RightBrace) {
            then_body.push(self.parse_statement()?);
            self.skip_newlines();
        }

        self.expect(Token::RightBrace)?;
        self.skip_newlines();

        let else_body = if matches!(self.current_token(), Token::Else) {
            self.advance();
            self.skip_newlines();
            self.expect(Token::LeftBrace)?;
            self.skip_newlines();

            let mut body = Vec::new();
            while !matches!(self.current_token(), Token::RightBrace) {
                body.push(self.parse_statement()?);
                self.skip_newlines();
            }

            self.expect(Token::RightBrace)?;
            Some(body)
        } else {
            None
        };

        Ok(Statement::If {
            condition,
            then_body,
            else_body,
        })
    }

    fn parse_for(&mut self) -> crate::error::Result<Statement> {
        self.expect(Token::For)?;

        let condition = if matches!(self.current_token(), Token::LeftBrace) {
            None
        } else {
            Some(self.parse_expression())
        };

        self.skip_newlines();
        self.expect(Token::LeftBrace)?;
        self.skip_newlines();

        let mut body = Vec::new();
        while !matches!(self.current_token(), Token::RightBrace) {
            body.push(self.parse_statement()?);
            self.skip_newlines();
        }

        self.expect(Token::RightBrace)?;

        Ok(Statement::For {
            init: None,
            condition,
            post: None,
            body,
        })
    }

    fn parse_return(&mut self) -> crate::error::Result<Statement> {
        self.expect(Token::Return)?;

        let value = if matches!(self.current_token(), Token::Newline | Token::RightBrace) {
            None
        } else {
            Some(self.parse_expression())
        };

        Ok(Statement::Return(value))
    }

    fn parse_asm(&mut self) -> crate::error::Result<Statement> {
        use crate::ast::AsmPart;
        
        self.expect(Token::Asm)?;
        
        if let Token::String(code) = self.current_token() {
            let asm_code = code.clone();
            self.advance();
            let parts = self.parse_asm_interpolation(&asm_code);
            Ok(Statement::InlineAsm { parts })
        } else if matches!(self.current_token(), Token::LeftBrace) {
            self.advance();
            self.skip_newlines();
            
            use crate::ast::AsmPart;
            let mut parts = Vec::new();
            
            while !matches!(self.current_token(), Token::RightBrace) {
                match self.current_token() {
                    Token::Dollar => {
                        // Handle $(varname)
                        self.advance();
                        if matches!(self.current_token(), Token::LeftParen) {
                            self.advance();
                            if let Token::Identifier(var_name) = self.current_token() {
                                parts.push(AsmPart::Variable(var_name.clone()));
                                self.advance();
                                self.expect(Token::RightParen)?;
                            }
                        }
                    }
                    Token::Identifier(instr) => {
                        let instr_name = instr.clone();
                        parts.push(AsmPart::Literal(instr_name));
                        self.advance();
                    }
                    Token::Number(n) => {
                        parts.push(AsmPart::Literal(format!(" {}", n)));
                        self.advance();
                    }
                    Token::Newline => {
                        parts.push(AsmPart::Literal("\n".to_string()));
                        self.advance();
                    }
                    _ => {
                        self.advance();
                    }
                }
            }
            
            self.expect(Token::RightBrace)?;
            Ok(Statement::InlineAsm { parts })
        } else {
            Err(self.error("expected assembly code string or block after 'asm'".to_string()))
        }
    }
    
    fn parse_asm_interpolation(&self, code: &str) -> Vec<crate::ast::AsmPart> {
        use crate::ast::AsmPart;
        
        let mut parts = Vec::new();
        let mut current_literal = String::new();
        let mut chars = code.chars().peekable();
        
        while let Some(ch) = chars.next() {
            if ch == '$' && chars.peek() == Some(&'(') {
                chars.next(); // consume '('
                
                // Save current literal if any
                if !current_literal.is_empty() {
                    parts.push(AsmPart::Literal(current_literal.clone()));
                    current_literal.clear();
                }
                
                // Extract variable name
                let mut var_name = String::new();
                while let Some(ch) = chars.next() {
                    if ch == ')' {
                        break;
                    }
                    var_name.push(ch);
                }
                
                parts.push(AsmPart::Variable(var_name.trim().to_string()));
            } else {
                current_literal.push(ch);
            }
        }
        
        // Add remaining literal if any
        if !current_literal.is_empty() {
            parts.push(AsmPart::Literal(current_literal));
        }
        
        parts
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Expression {
        let mut left = self.parse_and();

        while matches!(self.current_token(), Token::Or) {
            self.advance();
            let right = self.parse_and();
            left = Expression::Binary {
                op: BinaryOp::Or,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    fn parse_and(&mut self) -> Expression {
        let mut left = self.parse_equality();

        while matches!(self.current_token(), Token::And) {
            self.advance();
            let right = self.parse_equality();
            left = Expression::Binary {
                op: BinaryOp::And,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    fn parse_equality(&mut self) -> Expression {
        let mut left = self.parse_comparison();

        loop {
            let op = match self.current_token() {
                Token::Equal => BinaryOp::Equal,
                Token::NotEqual => BinaryOp::NotEqual,
                _ => break,
            };

            self.advance();
            let right = self.parse_comparison();
            left = Expression::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    fn parse_comparison(&mut self) -> Expression {
        let mut left = self.parse_additive();

        loop {
            let op = match self.current_token() {
                Token::Less => BinaryOp::Less,
                Token::LessEqual => BinaryOp::LessEqual,
                Token::Greater => BinaryOp::Greater,
                Token::GreaterEqual => BinaryOp::GreaterEqual,
                _ => break,
            };

            self.advance();
            let right = self.parse_additive();
            left = Expression::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    fn parse_additive(&mut self) -> Expression {
        let mut left = self.parse_multiplicative();

        loop {
            let op = match self.current_token() {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                Token::DoublePlus => BinaryOp::Concat,
                _ => break,
            };

            self.advance();
            let right = self.parse_multiplicative();
            left = Expression::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    fn parse_multiplicative(&mut self) -> Expression {
        let mut left = self.parse_unary();

        loop {
            let op = match self.current_token() {
                Token::Star => BinaryOp::Mul,
                Token::Slash => BinaryOp::Div,
                Token::Percent => BinaryOp::Mod,
                _ => break,
            };

            self.advance();
            let right = self.parse_unary();
            left = Expression::Binary {
                op,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        left
    }

    fn parse_unary(&mut self) -> Expression {
        match self.current_token() {
            Token::Minus => {
                self.advance();
                let operand = self.parse_unary();
                Expression::Unary {
                    op: UnaryOp::Neg,
                    operand: Box::new(operand),
                }
            }
            Token::Not => {
                self.advance();
                let operand = self.parse_unary();
                Expression::Unary {
                    op: UnaryOp::Not,
                    operand: Box::new(operand),
                }
            }
            Token::Ampersand => {
                self.advance();
                let operand = self.parse_unary();
                Expression::AddressOf {
                    operand: Box::new(operand),
                }
            }
            Token::Star => {
                self.advance();
                let operand = self.parse_unary();
                Expression::Deref {
                    operand: Box::new(operand),
                }
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_template_string(&mut self, s: String) -> Expression {
        use crate::ast::{TemplateStringPart, FormatSpec, FormatType};
        
        let mut parts = Vec::new();
        let mut current_literal = String::new();
        let mut chars = s.chars().peekable();
        
        while let Some(ch) = chars.next() {
            if ch == '$' && chars.peek() == Some(&'(') {
                // Found interpolation start
                chars.next(); // consume '('
                
                // Save current literal if any
                if !current_literal.is_empty() {
                    parts.push(TemplateStringPart::Literal(current_literal.clone()));
                    current_literal.clear();
                }
                
                // Extract expression string
                let mut expr_str = String::new();
                let mut paren_depth = 1;
                
                while let Some(ch) = chars.next() {
                    if ch == '(' {
                        paren_depth += 1;
                        expr_str.push(ch);
                    } else if ch == ')' {
                        paren_depth -= 1;
                        if paren_depth == 0 {
                            break;
                        }
                        expr_str.push(ch);
                    } else {
                        expr_str.push(ch);
                    }
                }
                
                // Parse format specifier if present (e.g., "value:08x")
                let (expr_str, format_spec) = self.parse_format_spec(&expr_str);
                
                // Parse the expression
                let mut lexer = crate::lexer::Lexer::new(&expr_str);
                let tokens = lexer.tokenize();
                let mut parser = Parser::new(tokens, &self.file);
                let expr = parser.parse_expression();
                
                parts.push(TemplateStringPart::Expression {
                    expr: Box::new(expr),
                    format: format_spec,
                });
            } else {
                current_literal.push(ch);
            }
        }
        
        // Add remaining literal if any
        if !current_literal.is_empty() {
            parts.push(TemplateStringPart::Literal(current_literal));
        }
        
        Expression::TemplateString { parts }
    }
    
    fn parse_format_spec(&self, expr_str: &str) -> (String, Option<crate::ast::FormatSpec>) {
        use crate::ast::{FormatSpec, FormatType};
        
        // Look for format specifier after colon, e.g., "x:08d" or "x:x"
        if let Some(colon_pos) = expr_str.rfind(':') {
            let expr_part = expr_str[..colon_pos].trim();
            let format_part = expr_str[colon_pos + 1..].trim();
            
            if !format_part.is_empty() {
                let mut width = None;
                let mut padding = ' ';
                let mut format_type = FormatType::Auto;
                
                let mut format_chars = format_part.chars().peekable();
                
                // Check for zero padding
                if format_chars.peek() == Some(&'0') {
                    padding = '0';
                    format_chars.next();
                }
                
                // Parse width
                let mut width_str = String::new();
                while let Some(&ch) = format_chars.peek() {
                    if ch.is_ascii_digit() {
                        width_str.push(ch);
                        format_chars.next();
                    } else {
                        break;
                    }
                }
                
                if !width_str.is_empty() {
                    width = width_str.parse().ok();
                }
                
                // Parse format type
                if let Some(ch) = format_chars.next() {
                    format_type = match ch {
                        'd' => FormatType::Decimal,
                        'x' => FormatType::Hex,
                        'X' => FormatType::HexUpper,
                        's' => FormatType::String,
                        _ => FormatType::Auto,
                    };
                }
                
                return (expr_part.to_string(), Some(FormatSpec {
                    width,
                    precision: None,
                    format_type,
                    padding,
                }));
            }
        }
        
        (expr_str.to_string(), None)
    }

    fn parse_primary(&mut self) -> Expression {
        match self.current_token().clone() {
            Token::Number(n) => {
                self.advance();
                Expression::Number(n)
            }
            Token::String(s) => {
                self.advance();

                if matches!(self.current_token(), Token::LBracket) {
                    self.advance();
                    let index = self.parse_expression();
                    if let Err(_) = self.expect(Token::RBracket) {
                        panic!("Expected closing bracket in string index");
                    }

                    return Expression::StringIndex {
                        string: Box::new(Expression::String(s)),
                        index: Box::new(index),
                    };
                }

                // Check if string contains interpolation syntax $(...)
                if s.contains("$(") {
                    self.parse_template_string(s)
                } else {
                    Expression::String(s)
                }
            }
            Token::Identifier(name) => {
                self.advance();

                if matches!(self.current_token(), Token::Dot) {
                    self.advance();
                    if let Token::Identifier(func_name) = self.current_token() {
                        let func_name = func_name.clone();
                        self.advance();

                        if matches!(self.current_token(), Token::LeftParen) {
                            self.advance();
                            let mut args = Vec::new();

                            while !matches!(self.current_token(), Token::RightParen) {
                                args.push(self.parse_expression());

                                if matches!(self.current_token(), Token::Comma) {
                                    self.advance();
                                }
                            }

                            if let Err(_) = self.expect(Token::RightParen) {
                                panic!("Expected closing parenthesis in module call");
                            }

                            return Expression::ModuleCall {
                                module: name,
                                function: func_name,
                                args,
                            };
                        }
                    }
                    panic!("Expected function name after module.");
                }

                if matches!(self.current_token(), Token::LeftParen) {
                    self.advance();
                    let mut args = Vec::new();

                    while !matches!(self.current_token(), Token::RightParen) {
                        args.push(self.parse_expression());

                        if matches!(self.current_token(), Token::Comma) {
                            self.advance();
                        }
                    }

                    if let Err(_) = self.expect(Token::RightParen) {
                        panic!("Expected closing parenthesis in function call");
                    }

                    // Special handling for eval() function
                    if name == "eval" && args.len() == 1 {
                        return Expression::Eval {
                            instruction: Box::new(args[0].clone()),
                        };
                    }

                    Expression::Call {
                        function: name,
                        args,
                    }
                } else if matches!(self.current_token(), Token::LBracket) {
                    self.advance();
                    let index = self.parse_expression();
                    if let Err(_) = self.expect(Token::RBracket) {
                        panic!("Expected closing bracket in array access");
                    }

                    Expression::ArrayAccess {
                        name,
                        index: Box::new(index),
                    }
                } else {
                    Expression::Identifier(name)
                }
            }
            Token::LeftParen => {
                self.advance();
                let expr = self.parse_expression();
                if let Err(_) = self.expect(Token::RightParen) {
                    panic!("Expected closing parenthesis");
                }
                expr
            }
            _ => panic!("Unexpected token: {:?}", self.current_token()),
        }
    }
}