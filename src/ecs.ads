--ecs.ads
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Unbounded.Hash;
with Components; use Components;
with IDs; use IDs;
with Graphics; use Graphics;

package ECS is

   function Hash_Component (Key : Component_Id) return Ada.Containers.Hash_Type;

   package Component_Map_Pkg is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type => Component_Id,
        Element_Type => Component_T'Class,
        Hash => Hash_Component,
        Equivalent_Keys => "=");
   subtype Component_Map is Component_Map_Pkg.Map;
   type Component_Map_Ptr is access Component_Map;

   type Components is record
      Components_Map : Component_Map;
   end record;
   type Components_Ptr is access all Components;

   procedure Add_Component (Self : in out Components;
                            Component : in Component_Id;
                            Component_Struct : in Component_T'Class);

   procedure Remove_Component (Self : in out Components;
                               Component : in Component_Id);

   function Get_Component (Self : in out Components;
                           Component : in Component_Id) return Component_T'Class;

   function Get_Component_Ptr (Self : Components_Ptr;
                               Component_Str : String)
                               return Component_Class_Ptr;

   function Has_Component (Self : in Components;
                           Component : in Component_Id) return Boolean;

   function Hash_Entity (Key : Entity_Id) return Ada.Containers.Hash_Type;

   package Entity_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Entity_Id,
      Element_Type    => Components_Ptr,
      Hash            => Hash_Entity,
      Equivalent_Keys => "=");
   subtype Entity_Components is Entity_Map.Map;
   type Entity_Components_Ptr is access all Entity_Components;
   --  Protected object for the entity list
   protected type Entity_Components_PO is
      --  "Reading" access is used for editing components of an entity,
      --    and allows multiple threads to access entities at once
      --  Writing access is used for adding/removing entities,
      --    and allows only 1 thread at once
      entry Claim_Reading (Entity_List : in out Entity_Components_Ptr);
      entry Claim_Writing (Entity_List : in out Entity_Components_Ptr);
      procedure Release_Reading;
      procedure Release_Writing;
   private
      Read_Using : Natural := 0;
      Write_Using : Boolean := False;
      Entities : aliased Entity_Components;
   end Entity_Components_PO;

   -- Add / Remove UML
   function Add_Entity (Self : in out Entity_Components_PO; Id : Entity_Id) return Components_Ptr;
   procedure Remove_Entity (Self : in out Entity_Components_PO; Id : Entity_Id);

   function Get_Entity_Components (Self : in Entity_Components; Id : Entity_Id) return Components_Ptr;

   function Get_Entities_Matching
     (Self : in Entity_Components; Required : Component_ID_Vector.Vector)
      return Entity_ID_Vector.Vector;

   --  Built-in systems
   -- ================================================================
   -- NEW: Added this system FIRST in the list
   -- Why: Detects when the terminal size changes
   -- How: Compares current size vs. previous size each frame
   -- When called: FIRST in main loop (before FlexLayoutSystem)
   -- ================================================================
   procedure TerminalResizeSystem (Entity_List_PO : in out Entity_Components_PO);

   -- EXISTING: Current systems, not changed.
   procedure FlexLayoutSystem (Entity_List_PO : in out Entity_Components_PO);
   procedure WidgetBackgroundSystem (Entity_List_PO : in out Entity_Components_PO);
   procedure TextRenderSystem (Entity_List_PO : in out Entity_Components_PO);
   procedure BufferCopySystem (Entity_List_PO : in out Entity_Components_PO);
   procedure BufferDrawSystem (Entity_List_PO : in out Entity_Components_PO);
   --  Renders all progress bar widgets to their buffers.
   --  Should be called after WidgetBackgroundSystem and before BufferCopySystem.
   procedure ProgressBarRenderSystem (Entity_List_PO : in out Entity_Components_PO);
   --  Swaps the double-buffering flag of Render_Info_Component_T
   --  Should be called after all other systems
   procedure DoubleBufferFlagSystem (Entity_List_PO : in out Entity_Components_PO);

   -- ================================================================
   -- NEW: Helper procedures for resize and widget movement
   -- How: Sets to Is_Dirty = True for all Flex_Layout_Component_T containers
   -- When: Called by TerminalResizeSystem when a resize is detected
   --Why did I separate this? Cleaner code, makes it REUSABLE
   -- ================================================================
   procedure Mark_All_Flex_Dirty (Entity_List : Entity_Components);

   -- Widget movement API for absolute positioning
   procedure Move_Widget (Entity_List : in out Entity_Components;
                         Widget_Entity : Entity_Id;
                         New_X : TUI_Width;
                         New_Y : TUI_Height);

   -- Simple API for relative movement (move by delta)
   procedure Move_Widget_By (Entity_List : in out Entity_Components;
                            Widget_Entity : Entity_Id;
                            Delta_X : Integer;
                            Delta_Y : Integer);

end ECS;
