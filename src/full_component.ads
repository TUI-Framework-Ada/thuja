with Ada.Strings.Unbounded;

package Full_Component is

   --  Easy access to unbounded strings
   package SU renames Ada.Strings.Unbounded;

   --  Entity_ID and an array type, remove these later once Entities and Components are merged
   --  Dining types for identifiers
   type Entity_ID is range 0 .. 99; -- Example ID (Change later)

   --  Defines a type within the record to hold a list of child entity IDs "<>"
   --  indicates unconstrained array
   type Entity_ID_Array is array (Positive range <>) of Entity_ID;

   --  Example color types, remove these later once Color and Pixel are pushed
   type Color is (Red, Green, Blue);



   --  Abstract component superclass
   type Component_T is abstract tagged limited null record;

   --  RenderInfoComponent
   type Render_Info_Component_T is new Component_T with record

      --  Data Fields
      BackBuffer      : Buffer_T;
      FrameBuffer     : Buffer_T;
      Terminal_Width  : Integer;
      Terminal_Height : Integer;

   end record;

   --  WidgetComponent
   type Widget_Component_T is new Component_T with record

      -- Position and Size
      Position_X : Integer := 0; --  Just set integers to default 0 values?
      Position_Y : Integer := 0;
      Size_Width : Integer := 0;
      Size_Height: Integer := 0;

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
      Text_Color : Color_Ptr; --  Color reference

   end record;

end Full_Component;
