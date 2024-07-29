with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Loadax_Run;

procedure Loadax is
begin
   if Argument_Count > 1 then
      Put_Line ("Usage: loadax [script]");
      Set_Exit_Status (64);
   elsif Argument_Count = 1 then
      null; -- from file
   else
      Loadax_Run.Run_Prompt;
   end if;
end Loadax;
