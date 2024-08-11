with Ada.Text_IO; use Ada.Text_IO;
with Scanner; use Scanner;
with Token; use Token;

package body Loadax_Run is
   Had_Error : Boolean := False;
   Input : Unbounded_String;

   procedure Run_Prompt is
   begin
      loop
         Put ("> ");
         Input := To_Unbounded_String (Get_Line);
         Run;
         Had_Error := False;
      end loop;
   exception
      when End_Error =>
         return;
   end Run_Prompt;

   procedure Run is
   begin
      Scan_Tokens;
      for T of Tokens loop
         Put_Line (To_String (T));
      end loop;
   end Run;

   procedure Error (Line : in Natural; Msg : in Unbounded_String) is
   begin
      Report (Line, To_Unbounded_String (""), Msg);
   end Error;

   procedure Report (Line : in Natural; Where : in Unbounded_String; Msg : in Unbounded_String) is
   begin
      Put_Line (Standard_Error,
                "[line " & Natural'Image (Line) & "] Error"
                & To_String (Where) & ": " & To_String (Msg));
      Had_Error := True;
   end Report;

end Loadax_Run;
