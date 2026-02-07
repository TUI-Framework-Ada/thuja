--ids.adb
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;
with Ada.Tags;
use Ada.Tags; --  Allow access to equality comparison operator on Tag type

package IDs is

   type Component_Id is new Ada.Strings.Unbounded.Unbounded_String; -- Component identifiers (for system queries later)
   type Entity_Id is new Ada.Strings.Unbounded.Unbounded_String; -- -- UML requires Entity_ID to be String, not Natural (Corrected)

   package Component_ID_Vector is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type => Natural,
        Element_Type => Component_Id);

   package Component_Tag_Vector is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type => Natural,
        Element_Type => Tag);

   package Entity_ID_Vector is new
     Ada.Containers.Indefinite_Vectors
       (Index_Type => Natural,
        Element_Type => Entity_Id);

   function To_CID (ID : String) return Component_Id;
   function To_EID (ID : String) return Entity_Id;

end IDs;
