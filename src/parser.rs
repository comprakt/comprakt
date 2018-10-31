

pub struct Parser {

}



impl Parser {

    fn parse_program(&self) {
        loop {
            if self.try_peek_eof() { break; }

            self.parse_class_declaration();
        }

        self.read_eof();
    }

    fn parse_class_declaration(&self) {
        self.read_keyword(Keyword.Class);
        self.read_identifier();

        self.read_op(Operator.LeftBracket);
        loop {
            if self.try_peek_op(Operator.RightBracket) { break; }

            self.parse_class_member();
        }
        self.read_op(Operator.RightBracket);
    }

    fn parse_class_member(&self) {
        self.read_keyword(Keyword.Public);

        self.try_read_keyword(Keyword.Static);
        self.parse_type();
        self.read_identifier();

        if self.try_peek(Operator.LeftParen) {
            self.read(Operator.RightParen);

            // method or main method
            loop {
                // parameters
                if self.try_peek(Operator.RightParen) { break; }
                self.parse_parameter();
            }

            self.read(Operator.RightParen);

            if (self.try_read_keyword(Keyword.Throws)) {
                self.read_identifier();
            }
            self.parse_block();
        }
        else {
            self.read_op(Operator.Semicolon);
            // we have a field
        }
    }

    fn parse_parameter(&self) {
        self.parse_type();
        self.read_identifier();
    }

    fn parse_type(&self) {
        parse_basic_type(&self);

        while self.try_peek_op(Operator.LeftBrace) { // [
            self.read_op(Operator.LeftBrace);
            self.read_op(Operator.RightBrace);
            // Array Type
        }
    }

    fn parse_basic_type(&self) {
        if self.try_peek_keyword(Keyword.Int) {
            self.try_peek_keyword(Keyword.Int);
            // int type
        }
        else if self.try_peek_keyword(Keyword.Boolean) {
            self.read_keyword(Keyword.Boolean);
            // bool type
        }
        else if self.try_peek_keyword(Keyword.Void) {
            self.read_keyword(Keyword.Void);
            // void type
        }
        else {
            // named type
            self.read_identifier();
        }
    }

    fn parse_block(&self) {
        self.read_op(Operator.LeftBracket);
        loop {
            if self.try_peek_op(Operator.RightBracket, true) { break; }

            self.parse_statement_or_local_var(true);
        }
        self.read_op(Operator.RightBracket);
    }

    fn parse_statement(&self) {
        self.parse_statement_or_local_var(false)
    }
    
    fn parse_statement_or_local_var(&self, allow_local_var_decl: bool) {
        if self.try_peek_op(Operator.LeftBracket) {
            // block
            parse_block();
        }
        else if self.try_peek_op(Operator.Semicolon) {
            // empty block
            self.read_op(Operator.Semicolon);
        }
        else if self.try_peek_keyword(Keyword.If) {
            self.read_keyword(Keyword.If);
            self.read_op(Operator.LeftParen);
            self.parse_expression();
            self.read_op(Operator.RightParen);

            self.parse_statement();
        }
        else if self.try_peek_keyword(Keyword.While) {
            self.read_keyword(Keyword.While);
            self.read_op(Operator.LeftParen);
            self.parse_expression();
            self.read(Operator.RightParen);
            self.parse_statement();
        }
        else if self.try_peek_keyword(Keyword.Return) {
            self.read_keyword(Keyword.Return);
            if !self.try_peek_op(Operator.Semicolon) {
                self.parse_expression();
            }
            self.read_op(Operator.Semicolon);
        }
        else if allow_local_var_decl {
            self.parse_type();
            self.read_identifier();
            if self.try_peek_op(Operator.Equals) {
                self.read_op(Operator.Equals);
                self.parse_expression();
            }
            self.read_op(Operator.Semicolon);
        }
        else {
            self.fail(&"Could not parse statement");
        }
    }

    fn parse_expression(&self) {
        parse_binary_expression(-1)
    }

    fn parse_binary_expression(&self, cur_priority: u32) {
        parse_expression();

        // tries to read some binary operators
        loop {
            if let Op(op, op_priority, associativity) = self.try_peek_binary_op() {
                if (op_priority > priority) break;
                
                self.read_op(op);
                let expr2 = self.parse_binary_expression(op_priority);
            }
            else {
                break;
            }
        }
    }

    fn parse_unary_expr(&self) {
        // try read prefixOp
        if self.try_peek_unary_op() {
            self.read_op();

            self.parse_unary_expr();
        }
        else {
            self.parse_postfix_expression();
        }
    }

    fn parse_postfix_expression(&self) {
        self.parse_primary_expression();

        loop {
            if self.try_peek_op(Operator.Dot) {
                self.read_identifier();

                if (self.try_peek_op(Operator.LeftParen)) {
                    self.parse_parenthesized_argument_list();
                    // method call: EXPR.ident(arg1, arg2, ...)
                }
                else {
                    // member reference: EXPR.ident
                }
            }
            else if self.try_peek_op(Operator.LeftBrace) {
                self.read_op(Operator.LeftBrace);
                self.parse_expression();
                self.read_op(Operator.RightBrace);
                // array access: EXPR[EXPR]
            }
            else {
                break;
            }
        }
    }

    fn parse_primary_expression(&self) {
        if self.try_peek_keyword(Keyword.Null) {
            self.read_keyword(Keyword.Null);
        }
        else if self.try_peek_keyword(Keyword.False) {
            self.read_keyword(Keyword.False);
        }
        else if self.try_peek_keyword(Keyword.True) {
            self.read_keyword(Keyword.True);
        }
        else if self.try_peek_integer() {
            self.read_integer();
        }
        else if self.try_peek_identifier() {
            self.read_identifier();

            if self.try_read_op(Operator.LeftParen) {
                // function call
                self.parse_parenthesized_argument_list();
            }
            else {
                // var ref
            }
        }
        else if self.try_peek_keyword(Keyword.This) {
            self.read_keyword(Keyword.This);
        }
        else if self.try_peek_op(Operator.LeftParen) {
            // parenthesized expression
            self.read_op(Operator.LeftParen);
            self.parse_expression();
            self.read(Operator.RightParen);
        }
        else if self.try_peek_keyword(Keyword.New) {
            self.read_keyword(Keyword.New);

            self.parse_basic_type();

            if true {
                // todo: "new int()" is not allowed
                // new object expression
                self.read_op(Operator.LeftParen);
                self.read(Operator.RightParen);
            }
            else {
                // new array expression
                self.read_op(Operator.LeftBrace);
                self.parse_expression();
                self.read_op(Operator.RightBrace);

                while self.try_peek_op(Operator.LeftBrace) { // [
                    self.read_op(Operator.LeftBrace);
                    self.read_op(Operator.RightBrace);
                }
            }
        }
        else {
            self.fail();
        }
    }

    fn parse_parenthesized_argument_list() {
        self.read_op(Operator.LeftParen);
        loop {
            if self.try_peek_op(Operator.RightParen) { break; }

            self.parse_expression();
        }
        self.read(Operator.RightParen);
    }
}
