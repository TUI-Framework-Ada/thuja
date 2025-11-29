with Ada.Strings.Unbounded;

package IDs is

   type Component_Id is new Ada.Strings.Unbounded.Unbounded_String; -- Component identifiers (for system queries later)
   type Entity_Id is new Ada.Strings.Unbounded.Unbounded_String; -- -- UML requires Entity_ID to be String, not Natural (Corrected)

end IDs;
