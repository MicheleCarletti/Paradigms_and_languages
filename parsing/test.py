#!/usr/bin/env python3
"""
@author  Michele Carletti
@license This software is free - http://www.gnu.org/licenses/gpl.html
Extend parse.py to other functions
"""

import re
from math import isclose
from syntree import *

class Tokenizer:
    def __init__(self, text):
        regex = r"\s*([A-Za-z0-9\.]+|.?)"
        self._tokens = re.finditer(regex, text.rstrip())
        self._next = next(self._tokens)

    def peek(self) -> str:
        return self._next.group(1)

    def consume(self, x):
        if self.peek() != x:
            raise SyntaxError("Expected " + x)
        self._next = next(self._tokens)

    def end(self):
        if self.peek():
            raise SyntaxError("Extra tokens")



# expr = term {( "+" | "-" ) term}
# term = factor {( "*" | "/" ) factor}
# factor = "-" factor | "(" expr ")" | identifier | number
# (identifiers start with a letter, numbers are float)


def logical_op(tok: Tokenizer) -> Expr:
    x = expr(tok)
    while tok.peek() in ("and", "or", "="):
        op = tok.peek()
        tok.consume(op)
        y = expr(tok)
        x = BinaryOp(op, x, y)
    return x

# expr = term {( "+" | "-" ) term}
def expr(tok: Tokenizer) -> Expr:
    x = term(tok)
    while tok.peek() in ("+", "-"):
        op = tok.peek()
        tok.consume(op)
        y = term(tok)
        x = BinaryOp(op, x, y)
    return x

# term = power {( "*" | "/" ) power}
def term(tok: Tokenizer) -> Expr:
    x = power(tok)
    while tok.peek() in ("*", "/"):
        op = tok.peek()
        tok.consume(op)
        y = power(tok)
        x = BinaryOp(op, x, y)
    return x

# power = factor {"^" factor}
def power(tok: Tokenizer) -> Expr:
    x = factor(tok)
    while tok.peek() == "^":
        op = tok.peek()
        tok.consume(op)
        y = factor(tok)
        x = BinaryOp(op, x, y)
    return x

# factor = "-" factor | "(" expr ")" | identifier | number
def factor(tok: Tokenizer) -> Expr:
    nxt = tok.peek()
    if nxt == "-":
        tok.consume("-")
        x = factor(tok)
        return UnaryOp("~", x)
    elif nxt == "not":
        tok.consume("not")
        x  = factor(tok)
        return UnaryOp("not", x)
    elif nxt == "exp":
        tok.consume("exp")
        x = factor(tok)
        return UnaryOp("exp", x)
    elif nxt == "(":
        tok.consume("(")
        x = logical_op(tok)
        tok.consume(")")
        return x      
    elif nxt in ["sin","cos","log","tan","sqrt"]:
        op = nxt
        tok.consume(nxt)
        x = factor(tok)
        return UnaryOp(op, x)  
    elif nxt.isalpha():
        tok.consume(nxt)
        return Var(nxt)
    else:
        tok.consume(nxt)
        return Num(float(nxt))


# Tests
def main():
    debug = True
    ctx = {"w": 0.0, "x": 1.0, "y": 1.5, "z": 0.5, "a": True, "b": False}

    tests = [("(((1.5)))", "1.5", 1.5),
             ("w * - z", "* w ~ z", 0.0),
             ("x / z * - y", "* / x z ~ y", -3.0),
             ("x / 0.5 * - - y", "* / x 0.5 ~ ~ y", 3.0),
             ("w", "w", 0.0),
             ("(x + w) * (x + y)", "* + x w + x y", 2.5),
             ("x ^ 2","^ x 2.0", 1.0),
             ("log x", "log x", 0.0),
             ("cos w", "cos w", 1.0),
             ("not a", "not a", False),
             ("a and b", "and a b", False),
             ("a or b", "or a b", True),
             ("sqrt x", "sqrt x", 1.0),
             ("tan 0.0", "tan 0.0", 0.0),
             ("exp w", "exp w", 1.0),
             ("a = b", "= a b", False)]

    for infix, prefix, val in tests:
        tok = Tokenizer(infix)
        ast = logical_op(tok)
        tok.end()
        if debug:
            print(f"Generated prefix: {ast.prefix()}")
            print(f"Generated eval: {ast.eval(ctx)}\n")
        assert ast.prefix() == prefix
        assert isclose(ast.eval(ctx), val)

if __name__ == "__main__":
    main()