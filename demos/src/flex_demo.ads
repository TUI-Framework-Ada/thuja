with Flexbox;

package Flex_Demo is

   ----------------------------------------------------------------------------
   --  Flex_Demo
   --
   --  Owns all non-ECS state and logic for the flexbox demonstration tab.
   --  Follows the same pattern as Text_Editor: pure state + operations with
   --  no ECS or rendering imports. Tab_Demo only does the ECS wiring.
   ----------------------------------------------------------------------------

   Num_Items  : constant := 4;
   Item_Basis : constant := 10;  --  base width of each child in chars

   --  Container dimensions — variables so H/h keys can resize height
   Con_W : Natural := 72;
   Con_H : Natural := 8;

   --  Current layout property indices, mutated by Next_Justify / Next_Align
   Justify_Idx : Natural := 0;
   Align_Idx   : Natural := 0;

   --  Fixed-length entity name strings (8 chars, matches To_EID calls)
   subtype Child_Name is String (1 .. 8);
   type Child_Name_Array is array (1 .. Num_Items) of Child_Name;
   Child_Names : constant Child_Name_Array :=
     ["flex_c_1", "flex_c_2", "flex_c_3", "flex_c_4"];

   --  All available justify and align values in cycle order
   type Justify_Cycle is array (0 .. 3) of Flexbox.Justify_Content;
   type Align_Cycle is array (0 .. 3) of Flexbox.Align_Items;
   All_Justifies : constant Justify_Cycle :=
     [Flexbox.Flex_Start, Flexbox.Center, Flexbox.Space_Between, Flexbox.Flex_End];
   All_Aligns    : constant Align_Cycle :=
     [Flexbox.Flex_Start, Flexbox.Center, Flexbox.Stretch, Flexbox.Flex_End];
   --  Advance to the next justify value (wraps)
   procedure Next_Justify;

   --  Advance to the next align value (wraps)
   procedure Next_Align;

   --  Grow the container width by 2, clamped to Max_W
   procedure Grow (Max_W : Natural);

   --  Shrink the container width by 2, clamped to minimum that fits all items
   procedure Shrink;

   --  Grow the container height by 2, clamped to Max_H
   procedure Grow_Height (Max_H : Natural);

   --  Shrink the container height by 2, minimum 4 rows
   procedure Shrink_Height;

   --  Human-readable name of the current justify setting (fixed-width, 13 chars)
   function Current_Justify_Name return String;

   --  Human-readable name of the current align setting (fixed-width, 10 chars)
   function Current_Align_Name return String;

   --  The active Justify_Content value for passing to ECS
   function Current_Justify return Flexbox.Justify_Content;

   --  The active Align_Items value for passing to ECS
   function Current_Align return Flexbox.Align_Items;

   --  Set initial container size based on terminal dimensions
   procedure Initialise (Width : Natural; Height : Natural);

end Flex_Demo;
