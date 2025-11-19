package Entities is
   type Entity_Id is new Natural;
   No_Entity : constant Entity_Id := 0;

   function Create return Entity_Id;
   procedure Destroy (Id : Entity_Id);
   function Is_Alive (Id : Entity_Id) return Boolean;
end Entities;
