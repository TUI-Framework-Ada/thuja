package body Components is

   procedure Add_Component (Self : in out Components;
                            Component_ID : in String;
                            Component_Struct : in Component_T_Ptr) is
   begin
      Self.Components_Map.Include(Component_ID, Component_Struct);
   end Add_Component;

   procedure Remove_Component (Self : in out Components;
                               Component_ID : in String) is
   begin
      Self.Components_Map.Exclude(Component_ID);
   end Remove_Component;

   function Get_Component (Self : in Components;
                           Component_ID : in String)
                           return Component_T_Ptr is
   begin
      return Self.Components_Map (Component_ID);
   end Get_Component;

end Components;
