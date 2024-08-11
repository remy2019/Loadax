with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Loadax_Run is

   procedure Run_Prompt;
   procedure Run;
   procedure Error (Line : in Natural; Msg : in Unbounded_String);
   procedure Report (Line : in Natural; Where : in Unbounded_String; Msg : in Unbounded_String);

end Loadax_Run;
