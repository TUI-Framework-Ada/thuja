with Graphics; use Graphics;
with Components; use Components;
with ECS; use ECS;
with IDs; use IDs;

package User_Library is

   type RainbowTextComponent is new Components.Component_T with record
      Hue_Change_Speed : Integer := 0;
   end record;

   procedure RainbowTextSystem (Entity_List : ECS.Entity_Components);

   --  Creates a progress bar entity with all necessary components.
   --  Parameters:
   --    Entity_List  - The ECS entity map to add the widget to
   --    E_ID         - Unique identifier for this widget
   --    Pos_X        - X position on screen
   --    Pos_Y        - Y position on screen
   --    Width        - Width of the progress bar (including borders)
   --    Height       - Height (typically 1 for a single-line bar)
   --    Filled_Color - Color for the filled portion
   --    Empty_Color  - Color for the empty portion
   --    BG_Color     - Background color
   procedure Create_Progress_Bar
     (Entity_List  : in Out Entity_Components;
      E_ID         : in Entity_Id;
      Pos_X        : in TUI_Width;
      Pos_Y        : in TUI_Height;
      Width        : in TUI_Width;
      Height       : in TUI_Height := 1;
      Filled_Color : in Color_t := Green;
      Empty_Color  : in Color_t := Gray;
      BG_Color     : in Color_t := Black);

   --  Sets the progress value (clamped to 0.0 .. 1.0)
   procedure Set_Progress
     (Entity_List : in Out Entity_Components;
      E_ID        : in Entity_Id;
      Value       : in Float);

   --  Gets the current progress value
   function Get_Progress
     (Entity_List : in Entity_Components;
      E_ID        : in Entity_Id) return Float;

   --  Increments progress by a given amount
   procedure Increment_Progress
     (Entity_List : in Out Entity_Components;
      E_ID        : in Entity_Id;
      Amount      : in Float);

   procedure Setup_Render_Info (Entities : in out Entity_Components;
                                Render_Info_ID : Entity_Id;
                                Term_Width : TUI_Width;
                                Term_Height : TUI_Height);

   procedure Setup_Root_Widget (Entities : in out Entity_Components;
                                Root_ID : Entity_Id;
                                Term_Width : TUI_Width;
                                Term_Height : TUI_Height;
                                Child_IDs : Entity_ID_Vector.Vector);

   procedure Run_Systems (Entities : in out Entity_Components);

   procedure Setup_Animation (Entities : in out Entity_Components;
                             Animation_ID : Entity_Id);

end User_Library;
