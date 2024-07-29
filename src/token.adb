package body Token is

   function To_String (Self : Literal) return String is
   begin
      return "";
   end To_String;

   function To_String (Self : String_Literal) return String is
   begin
      return To_String (Self.SL);
   end To_String;

   function To_String (Self : Numeric_Literal) return String is
   begin
      return Float'Image (Self.NL);
   end To_String;

   function To_String (Tk : Token) return String is
   begin
      return Token_Type'Image (Tk.T_Type) & " " &
             To_String (Tk.Lexeme) & " " & To_String (Tk.T_Literal);
   end To_String;

end Token;
