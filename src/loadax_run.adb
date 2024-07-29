with Ada.Text_IO; use Ada.Text_IO;

package body Loadax_Run is
   Had_Error : Boolean := False;

   procedure Run_Prompt is
   begin
      loop
         Put ("> ");
         Put_Line (Get_Line);
         null; -- run
         Had_Error := False;
      end loop;
   exception
      when End_Error =>
         return;
   end Run_Prompt;

end Loadax_Run;
