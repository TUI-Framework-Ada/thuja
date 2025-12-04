--  components.ads
--  Component definitions for the TUI ECS framework

with Ada.Strings.Unbounded;
with Graphics; use Graphics;
with IDs; use IDs;

package Components is

   --  Easy access to unbounded strings
   package SU renames Ada.Strings.Unbounded;

   --  Defines a type within the record to hold a list of child entity IDs "<>"
   --  indicates unconstrained array
   --type Entity_ID_Array is array (Positive range <>) of Entity_ID;

   --  Abstract component superclass
   type Component_T is abstract tagged null record;

   --  RenderInfoComponent
   type Render_Info_Component_T is new Component_T with record

      --  Data Fields
      Framebuffer     : Buffer_T;
      Backbuffer      : Buffer_T;
      Terminal_Width  : TUI_Width;
      Terminal_Height : TUI_Height;

   end record;

   --  WidgetComponent
   type Widget_Component_T is new Component_T with record

      -- Position and Size
      Position_X  : TUI_Width := TUI_Width'First;
      Position_Y  : TUI_Height := TUI_Height'First;
      Size_Width  : TUI_Width := TUI_Width'First;
      Size_Height : TUI_Height := TUI_Height'First;

      --  State flags
      Is_Visible : Boolean := True;   --  Set to true so it can be seen
      Is_Enabled : Boolean := True;   --  Set to true so it can function
      Has_Focus  : Boolean := False;  --  Set to false as all widgets cannot be in focus

      Render_Buffer : Buffer_T;  --  The buffer the widget renders its contents to
      Children      : Entity_ID_Vector.Vector;

   end record;

   --  TextComponent
   type Text_Component_T is new Component_T with record

      Text       : SU.Unbounded_String;  --  Unbounded string
      Text_Color : Color_t;              --  Color instance (copied, not referenced)

   end record;

   --  RootWidgetComponent
   type Root_Widget_Component_T is new Component_T with record
      null;
   end record;

   --  BackgroundColorComponent
   type Background_Color_Component_T is new Component_T with record
      Background_Color : Color_t;
   end record;

   ---------------------------------------------------------------------------
   --  Progress Bar Component
   ---------------------------------------------------------------------------
   --  Stores the state of a progress bar widget.
   --  Value ranges from 0.0 (empty) to 1.0 (full).

   type Progress_Bar_Component_T is new Component_T with record
      Value           : Float := 0.0;        --  Progress value (0.0 .. 1.0)
      Filled_Char     : Character := '=';    --  Character for filled portion
      Empty_Char      : Character := ' ';    --  Character for empty portion
      Filled_Color    : Color_t := Green;    --  Color of filled portion
      Empty_Color     : Color_t := Gray;     --  Color of empty portion
      Show_Percentage : Boolean := True;     --  Whether to show percentage text
      Border_Left     : Character := '[';    --  Left border character
      Border_Right    : Character := ']';    --  Right border character
   end record;

end Components;
