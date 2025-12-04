--  Thuja - Terminal User Interface Widget Library
--  Part of the TUI Framework using ECS architecture

with Graphics; use Graphics;
with Components; use Components;
with ECS; use ECS;
with IDs; use IDs;

package Thuja is

   ---------------------------------------------------------------------------
   --  Progress Bar Widget Creation
   ---------------------------------------------------------------------------

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

   ---------------------------------------------------------------------------
   --  Progress Bar Operations
   ---------------------------------------------------------------------------

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

   ---------------------------------------------------------------------------
   --  Progress Bar System
   ---------------------------------------------------------------------------

   --  Renders all progress bar widgets to their buffers.
   --  Should be called after WidgetBackgroundSystem and before BufferCopySystem.
   procedure ProgressBarRenderSystem (Entity_List : in Out Entity_Components);

   ---------------------------------------------------------------------------
   --  Utility
   ---------------------------------------------------------------------------

   --  Debug logging (original procedure)
   procedure Log;

end Thuja;