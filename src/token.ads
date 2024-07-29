with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Token is

   type Token_Type is (
      -- Single-character tokens
      TT_LEFT_PAREN, TT_RIGHT_PAREN, TT_LEFT_BRACE, TT_RIGHT_BRACE,
      TT_COMMA, TT_DOT, TT_MINUS, TT_PLUS, TT_SEMICOLON, TT_SLASH, TT_STAR,

      -- One or two character tokens
      TT_BANG, TT_BANG_EQUAL, TT_EQUAL, TT_EQUAL_EQUAL,
      TT_GREATER, TT_GREATER_EQUAL, TT_LESS, TT_LESS_EQUAL,

      -- Literals
      TT_IDENTIFIER, TT_STRING, TT_NUMBER,

      -- Keywords
      TT_AND, TT_CLASS, TT_ELSE, TT_FALSE, TT_FUN, TT_FOR, TT_IF, TT_NIL, TT_OR,
      TT_PRINT, TT_RETURN, TT_SUPER, TT_THIS, TT_TRUE, TT_VAR, TT_WHILE,

      TT_EOF);

   type Literal is tagged null record;

   type String_Literal is new Literal with record
      SL : Unbounded_String;
   end record;

   type Numeric_Literal is new Literal with record
      NL : Float;
   end record;

   type Token is record
      T_Type : Token_Type;
      Lexeme : Unbounded_String;
      T_Literal : Literal;
      Line : Integer;
   end record;

   function To_String (Tk : Token) return String;

end Token;
