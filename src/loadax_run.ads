with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Loadax_Run is

   procedure Run_Prompt;
   procedure Run;
   procedure Error (Line : Natural; Msg : Unbounded_String);
   procedure Report (Line : Natural; Where : Unbounded_String; Msg : Unbounded_String);

end Loadax_Run;
