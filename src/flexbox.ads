package Flexbox is

   type Flex_Direction is (Row, Column);
   type Justify_Content is (Flex_Start, Center, Space_Between);
   type Align_Items is (Flex_Start, Center, Stretch);

   type Flex_Item is record
      Flex_Grow     : Float := 0.0;
      Flex_Basis    : Integer := 0;
      Computed_Size : Integer := 0;
      Position_X    : Integer := 0;
      Position_Y    : Integer := 0;
   end record;

   type Flex_Item_Array is array (Positive range <>) of Flex_Item;
   -- FLEXBOX REDESIGN: Changed from unconstrained array to pointer-based design
   -- Ada requires fixed-size arrays at compile time, but we need to allocate items at runtime
   -- Using Flex_Item_Array_Ptr allows dynamic allocation of the item array
   type Flex_Item_Array_Ptr is access Flex_Item_Array;

   type Flex_Container is record
      Width     : Integer;
      Height    : Integer;
      Direction : Flex_Direction;
      Justify   : Justify_Content;
      Align     : Align_Items;
      -- FLEXBOX REDESIGN: Items is now a pointer, allowing runtime allocation
      Items     : Flex_Item_Array_Ptr;
      -- FLEXBOX REDESIGN: Item_Count tracks actual number of items (since array is dynamically allocated)
      Item_Count : Natural := 0;
   end record;

   procedure Layout (Container : in out Flex_Container);

end Flexbox;
