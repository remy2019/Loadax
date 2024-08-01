-- Start and Current starts from 1. so length check changes
with Ada.Containers.Indefinite_Ordered_Maps;

with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;

with Loadax;

package body Scanner is

   package Keyword_Map is new
      Ada.Containers.Indefinite_Ordered_Maps
         (Key_Type => String,
          Element_Type => Token_Type);

   Tokens : Token_Vector.Vector;

   Keywords : Keyword_Map.Map;

   procedure Scan_Tokens is
      procedure Init_Keywords is
      begin
         Keywords.Include ("and", TT_AND);
         Keywords.Include ("class", TT_CLASS);
         Keywords.Include ("else", TT_ELSE);
         Keywords.Include ("false", TT_FALSE);
         Keywords.Include ("for", TT_FOR);
         Keywords.Include ("fun", TT_FUN);
         Keywords.Include ("if", TT_IF);
         Keywords.Include ("nil", TT_NIL);
         Keywords.Include ("or", TT_OR);
         Keywords.Include ("print", TT_PRINT);
         Keywords.Include ("return", TT_RETURN);
         Keywords.Include ("super", TT_SUPER);
         Keywords.Include ("this", TT_THIS);
         Keywords.Include ("true", TT_TRUE);
         Keywords.Include ("var", TT_VAR);
         Keywords.Include ("while", TT_WHILE);
      end Init_Keywords;
   begin
      Init_Keywords;
      
      while not Is_At_End loop
         Start := Current;
         Scan_Token;
      end loop;

      Tokens.Append ((T_Type => TT_EOF,
                      Lexeme => To_Unbounded_String (""),
                      T_Literal => Literal'(null),
                      Line => Line));
   end Scan_Tokens;

   procedure Scan_Token is
      C : Character;

      function Match (Expected : Character) return Boolean is
      begin
         if Is_At_End then return False; end if;
         if Source (Current) /= Expected then return False; end if;

         Current := Current + 1;
         return True;
      end Match;

   function Is_Alpha (C : Character) return Boolean is
      (
         (C >= 'a' and C <= 'z') or
         (C >= 'A' and C <= 'Z') or
          C = '_'
      );

   function Is_Digit (C : Character) return Boolean is (C >= '0' and C <= '9');

   function Is_Alpha_Numeric (C : Character) return Boolean is
      (Is_Alpha (C) or Is_Digit (C));

      procedure String is
      begin
         while Peek /= Quotation and not Is_At_End loop
            if Peek = LF then Line := Line + 1; end if;
         end loop;

         if Is_At_End then
            Loadax.Error (Line, "Unterminated string.");
            return;
         end if;

         Advance (C);

         -- Trim surroundings
         Add_Token (TT_String, Source (Start + 1, Current - 1));
      end String;

   procedure Number is
   begin
      while Is_Digit (Peek) loop
         Advance (C);
      end loop;

      if Peek = '.' and Is_Digit (Peek_Next) then
         Advance (C);
         while Is_Digit (Peek) loop
            Advance (C);
         end loop;
      end if;

      Add_Token (TT_Number, Float'Value (Source (Start .. Current)));
   end Number;

   procedure Identifier is
      Text : String := "";
      Is_Keyword : Boolean;
      T_Type : Token_Type;
   begin
      while Is_Alpha_Numeric (Peek) loop
         Advance (C);
      end loop;

      Text := Source (Start, Current);
      Is_Keyword := Keywords.Contains (Text);
      T_Type := (if Is_Keyword then Keywords (Text) else TT_IDENTIFIER);

      Add_Token (T_Type);
   end Identifier;

   begin
      Advance (C);

      case C is
         when '(' => Add_Token (TT_LEFT_PAREN);
         when ')' => Add_Token (TT_RIGHT_PAREN);
         when '{' => Add_Token (TT_LEFT_BRACE);
         when '}' => Add_Token (TT_RIGHT_BRACE);
         when ',' => Add_Token (TT_COMMA);
         when '.' => Add_Token (TT_DOT);
         when '-' => Add_Token (TT_MINUS);
         when '+' => Add_Token (TT_PLUS);
         when ';' => Add_Token (TT_SEMICOLON);
         when '*' => Add_Token (TT_STAR);
         when '!' =>
            Add_Token (if Match ('=') then TT_BANG_EQUAL else TT_BANG);
         when '=' =>
            Add_Token (if Match ('=') then TT_EQUAL_EQUAL else TT_EQUAL);
         when '<' =>
            Add_Token (if Match ('=') then TT_LESS_EQUAL else TT_LESS);
         when '>' =>
            Add_Token (if Match ('=') then TT_GREATER_EQUAL else TT_GREATER);
         when '/' =>
            if Match ('/') then
               while Peek /= LF and not is_At_End loop
                  Advance (C);
               end loop;
            else
               Add_Token (TT_SLASH);
            end if;
         when Space | CR | HT => null;
         when LF =>
            Line := Line + 1;
         when Quotation =>
            String;
         when others =>
            if Is_Digit (C) then
               Number;
            elsif Is_Alpha (C) then
               Identifier;
            else
               Loadax.Error (Line, "Unexpected character.");
            end if;
      end case;
   end Scan_Token;

   function Peek return Character is
   begin
      if Is_At_End then return NUL;
      else return Source (Current);
      end if;
   end Peek;

   function Peek_Next return Character is
      (if (Current + 1 >= Length (Source)) then NUL
       else Source (Current + 1));

   procedure Advance (C : out Character) is
   begin
      C := Source (Current);
      Current := Current + 1;
   end Advance;
   
   function Is_At_End return Boolean is (Current >= Length (Source));

   procedure Add_Token (TT : in Token_Type) is (Add_Token (TT, Literal'(null)));

   procedure Add_Token (TT : in Token_Type; Lit : in Literal) is
      Text : String := Source (Start .. Current);
   begin
      Tokens.Append ((T_Type => TT,
                      Lexeme => Text,
                      T_Literal => Lit,
                      Line => Line));
   end Add_Token;

end Scanner;
