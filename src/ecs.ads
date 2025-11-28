with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings;
with Ada.Strings.Hash;
with Graphics; use Graphics;
with Components; use Components;

package ECS is

   package Component_ID_Vector is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type => Natural,
        Element_Type => String);

   package Component_Map_Pkg is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type => String,
        Element_Type => Component_T'Class,
        Hash => Ada.Strings.Hash,
        Equivalent_Keys => "=");
   subtype Component_Map is Component_Map_Pkg.Map;
   type Component_Map_Ptr is access Component_Map;

   type Components is record
      Components_Map : Component_Map;
   end record;
   type Components_Ptr is access all Components;

   procedure Add_Component (Self : in out Components;
                            Component_ID : in String;
                            Component_Struct : in Component_T'Class);

   procedure Remove_Component (Self : in out Components;
                               Component_ID : in String);

   function Get_Component (Self : in Components;
                           Component_ID : in String) return Component_T'Class;

end ECS;
