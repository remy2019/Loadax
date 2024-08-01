with Ada.Strings.Unbounded;
use  Ada.Strings.Unbounded;

with Ada.Containers.Vectors;

with Token;
use Token;

package Scanner is
   package Token_Vector is new
      Ada.Containers.Vectors
         (Index_Type => Natural,
          Element_Type => Token.Token);
   procedure Scan_Tokens;
private
   Source : Unbounded_String;

   Start : Natural := 1;
   Current : Natural := 1;
   Line : Natural := 1;

   procedure Scan_Token;
   function Peek return Character;
   function Peek_Next return Character;
   procedure Advance (C : out Character);
   function Is_At_End return Boolean;
   procedure Add_Token (TT : in Token_Type);
   procedure Add_Token (TT : in Token_Type; Lit : in Literal);

end Scanner;
