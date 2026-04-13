package body Flex_Demo is

   procedure Next_Justify is
   begin
      Justify_Idx := (Justify_Idx + 1) mod 4;
   end Next_Justify;

   procedure Next_Align is
   begin
      Align_Idx := (Align_Idx + 1) mod 4;
   end Next_Align;

   procedure Grow (Max_W : Natural) is
   begin
      if Con_W + 2 <= Max_W then
         Con_W := Con_W + 2;
      end if;
   end Grow;

   procedure Shrink is
      Min_W : constant Natural := Num_Items * Item_Basis + 2;
   begin
      if Con_W - 2 >= Min_W then
         Con_W := Con_W - 2;
      end if;
   end Shrink;

   procedure Grow_Height (Max_H : Natural) is
   begin
      if Con_H + 2 <= Max_H then
         Con_H := Con_H + 2;
      end if;
   end Grow_Height;

   procedure Shrink_Height is
      Min_H : constant Natural := 4;
   begin
      if Con_H - 2 >= Min_H then
         Con_H := Con_H - 2;
      end if;
   end Shrink_Height;

   function Current_Justify_Name return String is
   begin
      case All_Justifies (Justify_Idx) is
         when Flexbox.Flex_Start    =>
            return "Flex_Start   ";

         when Flexbox.Center        =>
            return "Center       ";

         when Flexbox.Space_Between =>
            return "Space_Between";
         
         when Flexbox.Flex_End      =>
            return "Flex_End     ";
      end case;
   end Current_Justify_Name;

   function Current_Align_Name return String is
   begin
      case All_Aligns (Align_Idx) is
         when Flexbox.Flex_Start =>
            return "Flex_Start";

         when Flexbox.Center     =>
            return "Center    ";

         when Flexbox.Stretch    =>
            return "Stretch   ";
         
         when Flexbox.Flex_End   =>
            return "Flex_End  ";
      end case;
   end Current_Align_Name;

   function Current_Justify return Flexbox.Justify_Content is
   begin
      return All_Justifies (Justify_Idx);
   end Current_Justify;

   function Current_Align return Flexbox.Align_Items is
   begin
      return All_Aligns (Align_Idx);
   end Current_Align;

end Flex_Demo;