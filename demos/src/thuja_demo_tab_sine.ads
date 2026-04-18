with Standardized_tab_interface;
with ECS;
with Graphics; use Graphics;

package Thuja_demo_tab_sine is

   ----------------------------------------------------------------------------
   --  Thuja_demo_tab_sine
   --
   --  PURPOSE
   --  -------
   --  Implements the Sine Wave Visualiser as a tab within Thuja_Demo.
   --  Wraps the logic from Sine_Wave_Demo into the Standardized_tab_interface
   --  so it can be hosted alongside HTop, Text Editor, Flexbox, and Sort tabs.
   --
   --  KEYBINDINGS (active when this tab is selected)
   --  -----------------------------------------------
   --  w / s   Increase / decrease amplitude
   --  a / d   Decrease / increase frequency
   --  q / e   Decrease / increase scroll speed
   ----------------------------------------------------------------------------

   type Tab_T is new Standardized_tab_interface.Tab_T with private;

   overriding
   procedure Create_Entities
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Content_Top : in TUI_Height;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height);

   overriding
   procedure Update
     (Tab         : in out Tab_T;
      World       : in out ECS.Entity_Components_PO;
      Term_Width  : in TUI_Width;
      Term_Height : in TUI_Height);

   --  Called from Thuja_Demo's main event loop when this tab is active.
   --  Not part of the abstract interface — just a regular concrete procedure.
   procedure Handle_Input (Tab : in out Tab_T; Char : in Character);

private

   Amp_Step   : constant Float := 0.1;
   Freq_Step  : constant Float := 0.1;
   Speed_Step : constant Float := 0.1;

   Amp_Min   : constant Float := 0.1;
   Amp_Max   : constant Float := 1.0;
   Freq_Min  : constant Float := 0.1;
   Freq_Max  : constant Float := 5.0;
   Speed_Min : constant Float := 0.1;
   Speed_Max : constant Float := 5.0;

   type Tab_T is new Standardized_tab_interface.Tab_T with record
      Amplitude    : Float := 1.0;
      Frequency    : Float := 2.5;
      Speed        : Float := 1.0;
      Phase        : Float := -3.141_592_65 / 2.0;
      Graph_Width  : Positive := 80;
      Graph_Height : Positive := 20;
      Tab_Index    : Natural := 4;
   end record;

end Thuja_demo_tab_sine;

