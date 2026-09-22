##################### BOILERPLATE BEGINS ############################# 
import sys

# Token types enumeration
##################### YOU CAN CHANGE THE ENUMERATION IF YOU WANT #######################
class TokenType:
    IDENTIFIER: str = "IDENTIFIER"
    KEYWORD: str = "KEYWORD"
    INTEGER: str = "INTEGER"
    FLOAT: str = "FLOAT"
    SYMBOL: str = "SYMBOL"

# Token hierarchy dictionary
token_hierarchy = {
    "move": TokenType.KEYWORD,
    "turn": TokenType.KEYWORD,
    "left": TokenType.KEYWORD,
    "right": TokenType.KEYWORD,
    "set": TokenType.KEYWORD,
    "print": TokenType.KEYWORD,
    "if": TokenType.KEYWORD,
    "else": TokenType.KEYWORD
}

# Valid symbols for this language
VALID_SYMBOLS = {";", "=", "<", ">"}

# helper function to check if it is a valid identifier
def is_valid_identifier(lexeme: str) -> bool:
    if not lexeme:
        return False
    # Check if the first character is an underscore or a letter
    if not (lexeme[0].isalpha() or lexeme[0] == '_'):
        return False
    # Check the rest of the characters (can be letters, digits, or underscores)
    for char in lexeme[1:]:
        if not (char.isalnum() or char == '_'):
            return False
    return True

# Tokenizer function
def tokenize(source_code: str) -> list[tuple[str, str]]:
    tokens: list[tuple[str, str]] = []
    position: int = 0
    
    while position < len(source_code):
        # Helper function to check if a character is alphanumeric
        def is_alphanumeric(char: str) -> bool:
            return char.isalpha() or char.isdigit() or (char == '_')
            
        char = source_code[position]
        
        # Check for whitespace and skip it
        if char.isspace():
            position += 1
            continue
            
        # Identifier and Keyword recognition
        if char.isalpha() or char == '_':
            lexeme = char
            position += 1
            while position < len(source_code) and is_alphanumeric(source_code[position]):
                lexeme += source_code[position]
                position += 1
                
            if lexeme in token_hierarchy:
                token_type = token_hierarchy[lexeme]
            else:
                # check if it is a valid identifier
                if is_valid_identifier(lexeme):
                    token_type = TokenType.IDENTIFIER
                else:
                    raise ValueError(f"Invalid identifier: {lexeme}")
                    
        # Integer or Float recognition
        elif char.isdigit():
            lexeme = char
            position += 1
            is_float = False
            
            while position < len(source_code):
                next_char = source_code[position]
                
                # checking if it is a float, or a full-stop
                if next_char == '.':
                    if is_float:
                        # Catch malformed floats like 3.14.15
                        raise ValueError(f"Invalid float format near {lexeme}.")
                    
                    if (position + 1 < len(source_code)):
                        next_next_char = source_code[position+1]
                        if next_next_char.isdigit():
                            is_float = True
                        else:
                            break
                            
                # checking for illegal identifier
                elif is_alphanumeric(next_char) and not next_char.isdigit():
                    while position < len(source_code) and is_alphanumeric(source_code[position]):
                        lexeme += source_code[position]
                        position += 1
                    if not is_valid_identifier(lexeme):
                        raise ValueError(f"Invalid identifier: {str(lexeme)}\nIdentifier can't start with digits")
                elif not next_char.isdigit():
                    break
                    
                lexeme += next_char
                position += 1
                
            token_type = TokenType.FLOAT if is_float else TokenType.INTEGER
            
        # Symbol recognition
        else:
            # Handle multi-character symbol '=='
            if char == '=' and position + 1 < len(source_code) and source_code[position + 1] == '=':
                lexeme = "=="
                position += 2
            elif char in VALID_SYMBOLS:
                lexeme = char
                position += 1
            else:
                raise ValueError(f"Unrecognized character: {char}")
                
            token_type = TokenType.SYMBOL
            
        tokens.append((token_type, lexeme))
        
    return tokens

########################## BOILERPLATE ENDS ###########################

def checkGrammar(tokens: list[tuple[str,str]]):
    """
    Syntactic analysis (CFG parser) — TO BE IMPLEMENTED BY THE STUDENT.
    Grammar:
        S          -> P
        P          -> statement P | epsilon
        statement  -> Move | Turn | Set | Print | If
        Move       -> move x ;
        Turn       -> turn left ; | turn right ;
        Set        -> set id = x ;
        Print      -> print x ;
        If         -> if x RelOp x Move ElsePart
        ElsePart   -> else Move | epsilon
        RelOp      -> < | > | ==
        x          -> integer | float | id
    """
    # write the code for syntactical analysis in this function using Recursive Descent
    # You CAN use other helper functions and create your own helper functions if needed
    # Raise a SyntaxError("message") if the token stream violates the grammar.
    
    class Parser:
        def __init__(self, tokens):
            self.tokens = tokens
            self.pos = 0
            self.n = len(tokens)

        def peek(self):
            if self.pos < self.n:
                return self.tokens[self.pos]
            return None

        def accept(self, typ=None, val=None):
            tok = self.peek()
            if not tok:
                return False
            ttype, tval = tok
            if typ and ttype != typ:
                return False
            if val and tval != val:
                return False
            self.pos += 1
            return tok

        def expect(self, typ=None, val=None, msg=None):
            tok = self.peek()
            if not tok:
                raise SyntaxError(msg or f"Expected {val or typ}, but found end of input")
            ttype, tval = tok
            if typ and ttype != typ:
                raise SyntaxError(msg or f"Expected {typ}, found {ttype}")
            if val and tval != val:
                raise SyntaxError(msg or f"Expected {val}, found {tval}")
            self.pos += 1
            return tok

        # P -> statement P | epsilon
        def parse_P(self):
            while True:
                tok = self.peek()
                if not tok:
                    return
                ttype, tval = tok
                if ttype == TokenType.KEYWORD and tval in {"move","turn","set","print","if"}:
                    self.parse_statement()
                elif ttype == TokenType.KEYWORD and tval == "else":
                    # else cannot start a statement at top level
                    return
                else:
                    return

        def parse_statement(self):
            tok = self.peek()
            if not tok:
                raise SyntaxError("Unexpected end of input while parsing statement")
            ttype, tval = tok
            if ttype != TokenType.KEYWORD:
                raise SyntaxError("Expected a statement starting with a keyword")
            if tval == "move":
                self.parse_Move()
            elif tval == "turn":
                self.parse_Turn()
            elif tval == "set":
                self.parse_Set()
            elif tval == "print":
                self.parse_Print()
            elif tval == "if":
                self.parse_If()
            else:
                raise SyntaxError(f"Unexpected keyword {tval}")

        def parse_x(self):
            tok = self.peek()
            if not tok:
                raise SyntaxError("Expected integer, float, or identifier, but found end of input")
            ttype, tval = tok
            if ttype in {TokenType.INTEGER, TokenType.FLOAT, TokenType.IDENTIFIER}:
                self.pos += 1
                return
            raise SyntaxError("Expected integer, float, or identifier")

        def parse_Move(self):
            # move x ;
            self.expect(TokenType.KEYWORD, "move")
            tok = self.peek()
            if not tok:
                raise SyntaxError("Expected integer, float, or identifier after 'move'")
            if tok[0] in {TokenType.INTEGER, TokenType.FLOAT, TokenType.IDENTIFIER}:
                self.pos += 1
            else:
                raise SyntaxError("Expected integer, float, or identifier after 'move'")
            if not self.accept(TokenType.SYMBOL, ";"):
                raise SyntaxError("Expected ';' after move statement")

        def parse_Turn(self):
            # turn left ; | turn right ;
            self.expect(TokenType.KEYWORD, "turn")
            if self.accept(TokenType.KEYWORD, "left"):
                pass
            elif self.accept(TokenType.KEYWORD, "right"):
                pass
            else:
                raise SyntaxError("Expected 'left' or 'right' after 'turn'")
            if not self.accept(TokenType.SYMBOL, ";"):
                raise SyntaxError("Expected ';' after turn statement")

        def parse_Set(self):
            # set id = x ;
            self.expect(TokenType.KEYWORD, "set")
            tok = self.peek()
            if not tok:
                raise SyntaxError("Expected identifier after 'set'")
            if tok[0] != TokenType.IDENTIFIER:
                raise SyntaxError("Expected identifier after 'set'")
            self.pos += 1
            if not self.accept(TokenType.SYMBOL, "="):
                raise SyntaxError("Expected '=' after identifier in set statement")
            tok = self.peek()
            if not tok:
                raise SyntaxError("Expected integer, float, or identifier after '='")
            if tok[0] in {TokenType.INTEGER, TokenType.FLOAT, TokenType.IDENTIFIER}:
                self.pos += 1
            else:
                raise SyntaxError("Expected integer, float, or identifier after '='")
            if not self.accept(TokenType.SYMBOL, ";"):
                raise SyntaxError("Expected ';' after set statement")

        def parse_Print(self):
            # print x ;
            self.expect(TokenType.KEYWORD, "print")
            tok = self.peek()
            if not tok:
                raise SyntaxError("Expected integer, float, or identifier after 'print'")
            if tok[0] in {TokenType.INTEGER, TokenType.FLOAT, TokenType.IDENTIFIER}:
                self.pos += 1
            else:
                raise SyntaxError("Expected integer, float, or identifier after 'print'")
            if not self.accept(TokenType.SYMBOL, ";"):
                raise SyntaxError("Expected ';' after print statement")

        def parse_If(self):
            # if x RelOp x Move ElsePart
            self.expect(TokenType.KEYWORD, "if")
            tok = self.peek()
            if not tok:
                raise SyntaxError("Expected integer, float, or identifier after 'if'")
            if tok[0] in {TokenType.INTEGER, TokenType.FLOAT, TokenType.IDENTIFIER}:
                self.pos += 1
            else:
                raise SyntaxError("Expected integer, float, or identifier after 'if'")
            # RelOp
            tok = self.peek()
            if not tok:
                raise SyntaxError("Expected relational operator after condition")
            if tok[0] == TokenType.SYMBOL and tok[1] in {"<", ">", "=="}:
                self.pos += 1
            else:
                raise SyntaxError("Expected relational operator ('<', '>', or '==')")
            # x
            tok = self.peek()
            if not tok:
                raise SyntaxError("Expected integer, float, or identifier after relational operator")
            if tok[0] in {TokenType.INTEGER, TokenType.FLOAT, TokenType.IDENTIFIER}:
                self.pos += 1
            else:
                raise SyntaxError("Expected integer, float, or identifier after relational operator")
            # Move (immediately)
            next_tok = self.peek()
            if not next_tok:
                raise SyntaxError("Expected move statement after if condition")
            if next_tok[0] == TokenType.KEYWORD and next_tok[1] == "move":
                self.parse_Move()
            else:
                raise SyntaxError("Expected move statement after if condition")
            # ElsePart
            next_tok = self.peek()
            if next_tok and next_tok[0] == TokenType.KEYWORD and next_tok[1] == "else":
                # consume else
                self.pos += 1
                nxt = self.peek()
                if not nxt:
                    raise SyntaxError("Expected move statement after else")
                if nxt[0] == TokenType.KEYWORD and nxt[1] == "move":
                    self.parse_Move()
                else:
                    raise SyntaxError("Expected move statement after else")

    # Begin parsing
    parser = Parser(tokens)
    # If the very first token is else -> immediate syntax error
    first = parser.peek()
    if first and first[0] == TokenType.KEYWORD and first[1] == "else":
        raise SyntaxError("'else' occurs before 'if'")

    # Parse P (sequence of statements)
    parser.parse_P()

    # If tokens remain, it's an error
    if parser.pos != parser.n:
        rem = parser.peek()
        if rem and rem[0] == TokenType.KEYWORD and rem[1] == "else":
            raise SyntaxError("'else' occurs before 'if'")
        raise SyntaxError("Unexpected tokens after parsing")

    return True

# Test the tokenizer and parser
if __name__ == "__main__":
    # Read source code directly from standard input (stdin) for autograder compatibility
    source_code = sys.stdin.read().strip()
    
    # Exit gracefully if the program is empty
    if not source_code:
        sys.exit(0)
        
    try:
        # Step 1: Lexical Analysis
        tokens = tokenize(source_code)
        
        # Step 2: Print Tokens
        # The assignment dictates that all successfully recognized tokens must 
        # be printed BEFORE any Syntax Errors are thrown.
        for token in tokens:
            print(f"Token Type: {token[0]}, Token Value: {token[1]}")
            
        # Step 3: Syntactic Analysis
        checkGrammar(tokens)
        
    except ValueError as e:
        # Lexical Errors halt the program without printing partial tokens
        print(f"ValueError: {e}")
    except SyntaxError as e:
        # Syntactic Errors are printed after the token stream
        print(f"SyntaxError: {e}")