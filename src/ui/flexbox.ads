with IDs; use IDs;  -- Import IDs to link items to entities

package Flexbox is

   -------------------------------------------------------------------------
   -- Type Definitions
   -------------------------------------------------------------------------
   
   type Flex_Direction is (Row, Column);
   
   type Justify_Content is (Flex_Start, Center, Space_Between, Flex_End);
   
   type Align_Items is (Flex_Start, Center, Stretch, Flex_End);

   -------------------------------------------------------------------------
   -- Flex Item Record
   -------------------------------------------------------------------------
   
   type Flex_Item is record
      Related_Entity : Entity_Id;       -- Links this item to a specific game entity
      Flex_Grow      : Float   := 0.0;
      Flex_Shrink    : Float   := 1.0;
      Flex_Basis     : Integer := 0; 
      Computed_Size  : Integer := 0;
      Position_X     : Integer := 0;
      Position_Y     : Integer := 0; 
      Cross_Size     : Integer := 0; 
   end record;

   -------------------------------------------------------------------------
   -- Flex Item Array Types
   -------------------------------------------------------------------------
   
   type Flex_Item_Array is array (Positive range <>) of Flex_Item;
   
   type Flex_Item_Array_Ptr is access Flex_Item_Array;

   -------------------------------------------------------------------------
   -- Flex Container Record
   -------------------------------------------------------------------------
   
   type Flex_Container is record
      Width      : Integer;
      Height     : Integer;
      Direction  : Flex_Direction;
      Justify    : Justify_Content;
      Align      : Align_Items;
      Items      : Flex_Item_Array_Ptr;
      Item_Count : Natural := 0;
   end record;

   -------------------------------------------------------------------------
   -- Public Procedures
   -------------------------------------------------------------------------
   
   procedure Layout (Container : in out Flex_Container);

end Flexbox;
