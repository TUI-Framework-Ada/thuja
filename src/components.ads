with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings;
with Ada.Strings.Hash;
with Component_T;

package Components is

   --  debug for testing compilation

   --  abstract superclass Component_T
-- type Component_T is abstract tagged null record;
   --  example subclass component
-- type ClickCounterComponent is new Component_T with record
--    No_Of_Clicks : Integer := 0;
-- end record;
   --  public Component_T access type (should be defined in a common place)
-- type Component_T_Ptr is access Component_T;

   --  end debug for testing compilation

   package Component_Map_Pkg is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type => String,
        Element_Type => Component_T_Ptr,
        Hash => Ada.Strings.Hash,
        Equivalent_Keys => "=");
   subtype Component_Map is Component_Map_Pkg.Map;
   type Component_Map_Ptr is access Component_Map;

   type Components is record
      Components_Map : Component_Map;
   end record;
   type Components_Ptr is access Components;

   procedure Add_Component (Self : in out Components;
                            Component_ID : in String;
                            Component_Struct : in Component_T_Ptr);

   procedure Remove_Component (Self : in out Components;
                               Component_ID : in String);

   function Get_Component (Self : in Components;
                           Component_ID : in String) return Component_T_Ptr;

end Components;
