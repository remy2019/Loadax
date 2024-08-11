package body Token is

   function To_String (Self : Literal) return String is
      ((case Self.L_Type is
         when NIL => "",
         when STR => To_String (Self.L_Str),
         when NUM => Float'Image (Self.L_Num)
      ));

   function To_String (Tk : Token) return String is
   begin
      return Token_Type'Image (Tk.T_Type) & " " &
             To_String (Tk.Lexeme) & " " & To_String (Tk.T_Literal);
   end To_String;

end Token;
