

pub struct Parser {

}



impl Parser {
    fn parseProgram(&self) {
        loop {
            if (tryPeek(EOF)) {
                break;
            }

            parseClassDeclaration();
        }
    }

    fn parseClassDeclaration(&self) {
        self.read(Keyword.Class);
        let name = self.readIdentifier();


    }

    fn parseClassMember(&self) {
        self.read(Keyword.Public);

        let isStatic = self.tryRead(Keyword.Static);
        let type = self.parseType();
        let name = self.readIdentifier();

        // "(" oder ";"
        self.peek(
            option(Operator.LeftParen, |o| {
                // left paren
                parseList();
            }),
            option(Operator.Semicolon, |o| {
                self.read();
                // field
            })
        );
    }

    fn parseList() {

    }

    fn parseType(&self) {
        

    }

    fn parseStatement(&self) {
        self.peek(
            option(Operator.LeftBracket, |o| {
                self.read(Operator.LeftBracket);
                loop {

                }
                self.read(Operator.RightBracket);
            }),
            option(Operator.Semicolon, |o| {
                // Empty Statement
                self.read(Operator.Semicolon);
            }),
            option(Keyword.If, |o| {
                self.read(Keyword.If);
                self.read(Operator.LeftParen);
                self.parseExpression();
                self.read(Operator.RightParen);

                self.parseStatement();
            }),
            option(Keyword.While, |o| {
                self.read(Operator.LeftParen);
                self.parseExpression();
                self.read(Operator.RightParen);
                self.parseStatement();
            }),
            option(Keyword.Return, |o| {
                self.read(Keyword.Return);
                if !self.tryPeek(Operator.Semicolon) {
                    self.parseExpression();
                }
                self.read(Operator.Semicolon);
            }),
        );
    }

    fn parseExpression(&self, priority: u32 = -1) {
        let expr = parseExpression();

        loop {

            if let Op(op, op_priority, associativity) = self.tryPeekBinaryOperator() {
                if (op_priority > priority) break;
                
                self.read(op);
                let expr2 = self.parseExpr(op_priority);
            }
            else {
                break;
            }
        }
    }

    fn parseUnaryExpr(&self) {
        // try read prefixOp


    }

    fn parsePostfixExpression(&self) {
        let primary = parsePrimaryExpression();

        loop {
            parsePostfixOp();
        }
    }

    fn parsePrimaryExpression(&self) {
        self.peek(
            option(Keyword.Null, |o| {

            }),
            option(Keyword.False, |o| {

            }),
            option(Keyword.True, |o| {

            }),
            option(Keyword.This, |o| {

            }),
        );
    }

    fn parseNewExpression(&self) {
        self.read(Keyword.New);

    }
}
