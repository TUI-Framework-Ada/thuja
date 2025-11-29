with Ada.Strings.Unbounded;
with Graphics; use Graphics;
with IDs; use IDs;

package Components is

   --  Easy access to unbounded strings
   package SU renames Ada.Strings.Unbounded;

   --  Defines a type within the record to hold a list of child entity IDs "<>"
   --  indicates unconstrained array
   type Entity_ID_Array is array (Positive range <>) of Entity_ID;



   --  Abstract component superclass
   type Component_T is abstract tagged null record;

   --  RenderInfoComponent
   type Render_Info_Component_T is new Component_T with record

      --  Data Fields
      BackBuffer      : Buffer_T;
      FrameBuffer     : Buffer_T;
      Terminal_Width  : TUI_Width;
      Terminal_Height : TUI_Height;

   end record;

   --  WidgetComponent
   type Widget_Component_T is new Component_T with record

      -- Position and Size
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
      Children      : Entity_ID_Array (1 .. 0); --  Flexible array for children widgets, initial length 0

   end record;

   --  TextComponent
   type Text_Component_T is new Component_T with record

      Text      : SU.Unbounded_String; --  Unbounded string
      Text_Color : Color_t; --  Color instance (copied, not referenced)

   end record;

   --  RootWidgetComponent
   type Root_Widget_Component_T is new Component_T with record
      null;
   end record;

   --  BackgroundColorComponent
   type Background_Color_Component_T is new Component_T with record
      Background_Color : Color_t;
   end record;

end Components;
