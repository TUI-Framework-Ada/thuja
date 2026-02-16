--  components.ads
--  Component definitions for the TUI ECS framework

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Graphics; use Graphics;
with IDs; use IDs;
with Flexbox; use Flexbox;

package Components is

   --  Easy access to unbounded strings
   package SU renames Ada.Strings.Unbounded;

   --  Defines a type within the record to hold a list of child entity IDs "<>"
   --  indicates unconstrained array
   --type Entity_ID_Array is array (Positive range <>) of Entity_ID;

   --  Abstract component superclass
   type Component_T is abstract tagged null record;
   type Component_Class_Ptr is access all Component_T'Class;

   --  RenderInfoComponent
   -- UPDATED: Added Prev_Terminal_Width/Height for resize detection
   type Render_Info_Component_T is new Component_T with record
      --  Data Fields
      Framebuffer_1   : aliased Buffer_T;
      Framebuffer_2   : aliased Buffer_T; --  Double-buffering
      Drawing_FB      : Protected_DB_Ptr; --  Which FB the render thread should use
      Backbuffer      : Buffer_T;
      Terminal_Width  : TUI_Width;
      Terminal_Height : TUI_Height;

      -- NEW: Previous terminal size for detecting resize events
      -- How it works: Compare current size vs. previous size each frame
      -- If different, trigger resize handling in RenderSystem
      -- If same, no action needed
      Prev_Terminal_Width  : Natural := 0;
      Prev_Terminal_Height : Natural := 0;
   end record;

   -- FLEXBOX INTEGRATION: New component that attaches flexbox layout rules to an entity
   -- Entities with this component will have their positions/sizes computed by the flexbox algorithm
   -- The Is_Dirty flag prevents unnecessary recalculation every frame
   type Flex_Layout_Component_T is new Component_T with record
      Flex_Container : Flexbox.Flex_Container;  -- Holds flexbox layout configuration and items
      Is_Dirty : Boolean := True;               -- Marks if layout needs recalculation
   end record;

   -- NEW: Position Mode - Controls how a widget is positioned
   type Position_Mode is (
      Flex,      -- Positioned by parent's flexbox (default)
      Absolute,  -- Manual X/Y coordinates (ignore flexbox)
      Relative,  -- Manual offset from flexbox position
      Fixed      -- Fixed screen position (ignore parent)
   );

   -- NEW: Position Mode Component - Attach to widgets that need manual positioning
   -- How it works: Add this component to widgets you want to manually position
   -- Default is Flex: Most widgets use flexbox automatically
   -- FlexLayoutSystem checks this: If mode != Flex, skip the widget
   type Position_Mode_Component_T is new Component_T with record
      Mode : Position_Mode := Flex;
   end record;

   --  WidgetComponent
   type Widget_Component_T is new Component_T with record
      -- FLEXBOX INTEGRATION: Position_X/Y and Size_Width/Height are now computed by FlexLayoutSystem
      -- instead of being hardcoded, making layouts dynamic and responsive
      Position_X : TUI_Width := TUI_Width'First; --  Just set integers to default minimum values?
      Position_Y : TUI_Height := TUI_Height'First;
      Size_Width : TUI_Width := TUI_Width'First;
      Size_Height: TUI_Height := TUI_Height'First;

      --  State flags
      Is_Visible : Boolean := True; --  Set to true so it can be seen
      Is_Enabled : Boolean := True; --  Set to true so it can function
      Has_Focus  : Boolean := False; --  Set to false as all widgets cannot be in focus at
                                     --  same time
      Render_Buffer : Buffer_T; --  The buffer the widget renders its contents to
      --Children      : Entity_ID_Array (1 .. 0); --  Array for children widgets, length 0
      Children : Entity_ID_Vector.Vector;
   end record;

   --  TextComponent
   type Text_Component_T is new Component_T with record

      Text       : SU.Unbounded_String;  --  Unbounded string
      Text_Color : Color_t;              --  Color instance (copied, not referenced)

      -- Text Position Fields for relative offset from parent widget
      Offset_X  :  TUI_Width  := TUI_Width'First;
      Offset_Y  :  TUI_Height := TUI_Height'First;

      -- Formatting flags for text stylization
      Is_Bold          : Boolean := False;
      Is_Italic        : Boolean := False;
      Is_Underline     : Boolean := False;
      Is_Strikethrough : Boolean := False;

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

   --  Selectable Component
   --  Marks an entity as participating in Tab-cycle focus selection.
   --  Tab_Order controls the cycling sequence (0 = auto, nonzero = explicit).
   --  The actual focus state is stored in Widget_Component_T.Has_Focus.
   type Selectable_Component_T is new Component_T with record
      Tab_Order : Natural := 0;
   end record;

   --  Widget Command Entry, a single key sequence + display name pair
   type Widget_Command_Entry_T is record
      Keys : SU.Unbounded_String;
      Name : SU.Unbounded_String;
   end record;

   package Widget_Command_Vectors is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Widget_Command_Entry_T);

   --  Command Set Component
   --  Attach to a selectable widget to give it its own set of key-sequence commands.
   --  When the widget gains focus, these commands become active.
   --  When the widget loses focus, they are deactivated.
   type Command_Set_Component_T is new Component_T with record
      Commands : Widget_Command_Vectors.Vector;
   end record;

end Components;
