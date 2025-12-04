--ids.adb
package body IDs is

   function To_CID (ID : String) return Component_Id is
   begin
      return Component_Id (Ada.Strings.Unbounded.To_Unbounded_String (ID));
   end To_CID;

   function To_EID (ID : String) return Entity_Id is
   begin
      return Entity_Id (Ada.Strings.Unbounded.To_Unbounded_String (ID));
   end To_EID;

end IDs;
