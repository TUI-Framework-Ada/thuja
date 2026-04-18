--==============================================================================
-- ECS.ADS - Entity Component System Specification
--==============================================================================

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Finalization;
with Ada.Strings;
with Ada.Strings.Unbounded.Hash;
with Components; use Components;
with IDs;        use IDs;
with Graphics;   use Graphics;
with Ada.Tags;

package ECS is

   --===========================================================================
   -- COMPONENT HASH & STORAGE
   --===========================================================================

   function Hash_Component
     (Key : Component_Id) return Ada.Containers.Hash_Type;

   package Component_Map_Pkg is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Component_Id,
        Element_Type    => Component_T'Class,
        Hash            => Hash_Component,
        Equivalent_Keys => "=");
   subtype Component_Map is Component_Map_Pkg.Map;
   type Component_Map_Ptr is access Component_Map;

   protected type Components_PO is
      entry Claim_Element;
      entry Claim_List;
      procedure Release_Element;
      procedure Release_List;
   private
      Element_Using : Natural := 0;
      List_Using    : Boolean := False;
   end Components_PO;
   type Components_PO_Ptr is access all Components_PO;

   type Components is record
      Components_Map : Component_Map;
      PO             : aliased Components_PO;
   end record;
   type Components_Ptr is access all Components;

   --===========================================================================
   -- COMPONENT OPERATIONS
   --===========================================================================

   type Component_Class_Ref (Data : access Component_T'Class) is
     new Ada.Finalization.Controlled
   with record
      Entity : Components_PO_Ptr;
   end record
   with Implicit_Dereference => Data;

   procedure Initialize_Ref (Self : in out Component_Class_Ref);
   overriding
   procedure Adjust (Self : in out Component_Class_ref);
   overriding
   procedure Finalize (Self : in out Component_Class_Ref);

   procedure Add_Component
     (Self             : in out Components;
      Component        : in Component_Id;
      Component_Struct : in Component_T'Class);

   procedure Add_Component
     (Self             : in out Components;
      Component_Str    : in String;
      Component_Struct : in Component_T'Class);

   procedure Remove_Component
     (Self : in out Components; Component : in Component_Id);

   procedure Remove_Component
     (Self : in out Components; Component_Str : in String);

   procedure Remove_Component
     (Self : in out Components; Component_Tag : in Ada.Tags.Tag);

   function Get_Component
     (Self : in out Components; Component : in Component_Id)
      return Component_T'Class;

   function Get_Component
     (Self : in out Components; Component_Str : in String)
      return Component_T'Class;

   function Get_Component
     (Self : in Components; Component_Tag : in Ada.Tags.Tag)
      return Component_T'Class;

   function Get_Component_Ptr
     (Self : Components_Ptr; Component_Key : Component_Id)
      return Component_Class_Ref;

   function Get_Component_Ptr
     (Self : Components_Ptr; Component_Str : String)
      return Component_Class_Ref;

   function Get_Component_Ptr
     (Self : Components_Ptr; Component_Tag : in Ada.Tags.Tag)
      return Component_Class_Ref;

   function Get_Component_ID
     (Self : in Components; Component_Tag : in Ada.Tags.Tag)
      return Component_Id;

   function Get_Component_IDs
     (Self : in Components; Component_Tag : in Ada.Tags.Tag)
      return Component_ID_Vector.Vector;

   function Has_Component
     (Self : in Components; Component : in Component_Id) return Boolean;

   function Has_Component
     (Self : in Components; Component_Str : in String) return Boolean;

   function Has_Component
     (Self : in Components; Component_Tag : in Ada.Tags.Tag) return Boolean;

   --===========================================================================
   -- ENTITY HASH & STORAGE
   --===========================================================================

   function Hash_Entity (Key : Entity_Id) return Ada.Containers.Hash_Type;

   package Entity_Map is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => Entity_Id,
        Element_Type    => Components_Ptr,
        Hash            => Hash_Entity,
        Equivalent_Keys => "=");
   subtype Entity_Components is Entity_Map.Map;
   type Entity_Components_Ptr is access all Entity_Components;

   --===========================================================================
   -- ENTITY PROTECTED OBJECT (Thread-Safe Access)
   --===========================================================================

   protected type Entity_Components_PO is
      entry Claim_Reading (Entity_List : in out Entity_Components_Ptr);
      entry Claim_Writing (Entity_List : in out Entity_Components_Ptr);
      procedure Release_Reading;
      procedure Release_Writing;
   private
      Read_Using  : Natural := 0;
      Write_Using : Boolean := False;
      Entities    : aliased Entity_Components;
   end Entity_Components_PO;

   --===========================================================================
   -- ENTITY OPERATIONS
   --===========================================================================

   function Add_Entity
     (Self : in out Entity_Components_PO; Id : Entity_Id)
      return Components_Ptr;
   procedure Remove_Entity
     (Self : in out Entity_Components_PO; Id : Entity_Id);

   function Get_Entity_Components
     (Self : in Entity_Components; Id : Entity_Id) return Components_Ptr;

   function Get_Entities_Matching
     (Self : in Entity_Components; Required : Component_ID_Vector.Vector)
      return Entity_ID_Vector.Vector;

   function Get_Entities_Matching
     (Self : in Entity_Components; Required : Component_Tag_Vector.Vector)
      return Entity_ID_Vector.Vector;

   function Make_Widget
     (World : in out Entity_Components_PO;
      Name  : in String;
      X     : in TUI_Width;
      Y     : in TUI_Height;
      W     : in TUI_Width;
      H     : in TUI_Height) return Components_Ptr;

   function Make_Widget_With_BG
     (World : in out Entity_Components_PO;
      Name  : in String;
      X     : in TUI_Width;
      Y     : in TUI_Height;
      W     : in TUI_Width;
      H     : in TUI_Height;
      BG    : in Color_t) return Components_Ptr;

   procedure Initialize_World
     (World     : in out Entity_Components_PO;
      Width     : in TUI_Width;
      Height    : in TUI_Height;
      Tab_Count : in Natural := 0);

   --===========================================================================
   -- BUILT-IN SYSTEMS
   --===========================================================================
   type Tab_Direction is (Next, Prev);

   procedure TerminalResizeSystem (Entity_List_PO : in out Entity_Components_PO; New_Width : in TUI_Width; New_Height : in TUI_Height);
   procedure FlexLayoutSystem (Entity_List_PO : in out Entity_Components_PO);
   procedure FlexAlignTextSystem (Entity_List_PO : in out Entity_Components_PO);
   procedure ClearWidgetBufferSystem (Entity_List_PO : in out Entity_Components_PO);
   procedure WidgetBackgroundSystem (Entity_List_PO : in out Entity_Components_PO);
   procedure TextRenderSystem (Entity_List_PO : in out Entity_Components_PO);
   procedure BufferCopySystem (Entity_List_PO : in out Entity_Components_PO);
   procedure BufferDrawSystem (Entity_List_PO : in out Entity_Components_PO);
   procedure ProgressBarRenderSystem (Entity_List_PO : in out Entity_Components_PO);
   procedure DoubleBufferFlagSystem (Entity_List_PO : in out Entity_Components_PO);
   procedure ResetBackbufferSystem(Entity_List_PO : in out Entity_Components_PO);
   function Get_Active_Tab (Entity_List_PO : in out Entity_Components_PO) return Natural;
   procedure SelectionSystem(Entity_List_PO : in out Entity_Components_PO; Tab_Pressed : in Boolean);
   procedure TabInitSystem (Entity_List_PO : in out Entity_Components_PO);
   procedure TabSwitchSystem(Entity_List_PO : in out Entity_Components_PO; Direction : in Tab_Direction);
   
   --===========================================================================
   -- HELPER PROCEDURES
   --===========================================================================

   procedure Mark_All_Flex_Dirty (Entity_List : in out Entity_Components);

   procedure Move_Widget
     (Entity_List   : in out Entity_Components;
      Widget_Entity : Entity_Id;
      New_X         : TUI_Width;
      New_Y         : TUI_Height);

   procedure Move_Widget_By
     (Entity_List   : in out Entity_Components;
      Widget_Entity : Entity_Id;
      Delta_X       : Integer;
      Delta_Y       : Integer);

   procedure CalendarDisplaySystem
     (Entity_List_PO : in out Entity_Components_PO);

end ECS;
