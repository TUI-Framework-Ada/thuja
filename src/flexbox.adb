package body Flexbox is

   -- FLEXBOX REDESIGN: Layout algorithm computes positions and sizes for all flex items
   -- Based on CSS Flexbox spec: distributes free space, applies grow factors, and positions items
   procedure Layout (Container : in out Flex_Container) is
      Total_Basis : Integer := 0;
      Total_Grow  : Float := 0.0;
      Free_Space  : Integer := 0;
   begin
      -- FLEXBOX REDESIGN: Guard against null or empty items array
      if Container.Items = null or Container.Item_Count = 0 then
         return;
      end if;

      -- FLEXBOX ALGORITHM Step 1: Sum all basis sizes and grow factors
      for I in 1 .. Container.Item_Count loop
         Total_Basis := Total_Basis + Container.Items(I).Flex_Basis;
         Total_Grow  := Total_Grow + Container.Items(I).Flex_Grow;
      end loop;

      -- FLEXBOX ALGORITHM Step 2: Calculate free space available for distribution
      if Container.Direction = Row then
         Free_Space := Container.Width - Total_Basis;
      else
         Free_Space := Container.Height - Total_Basis;
      end if;

      -- FLEXBOX ALGORITHM Step 3: Distribute free space based on flex-grow factors
      for I in 1 .. Container.Item_Count loop
         if Total_Grow > 0.0 then
            -- Item gets its basis size plus a proportional share of free space
            Container.Items(I).Computed_Size :=
              Container.Items(I).Flex_Basis +
              Integer (Float (Free_Space) *
                       (Container.Items(I).Flex_Grow / Total_Grow));
         else
            -- No growth defined; item keeps its basis size
            Container.Items(I).Computed_Size :=
              Container.Items(I).Flex_Basis;
         end if;
      end loop;

      -- FLEXBOX ALGORITHM Step 4: Position items based on justify-content and direction
      declare
         Cursor : Integer := 0;
         Gap    : Integer := 0;
      begin
         case Container.Justify is
            when Center =>
               -- Center alignment: start cursor at half the free space
               Cursor := Free_Space / 2;
            when Space_Between =>
               -- Space items evenly with gaps between them
               if Container.Item_Count > 1 then
                  Gap := Free_Space / (Container.Item_Count - 1);
               end if;
            when Flex_Start =>
               -- Default: items start at the beginning
               null;
         end case;

         for I in 1 .. Container.Item_Count loop
            if Container.Direction = Row then
               -- Horizontal layout: position along X-axis
               Container.Items(I).Position_X := Cursor;
               Container.Items(I).Position_Y := 0;
               Cursor := Cursor + Container.Items(I).Computed_Size + Gap;
            else
               -- Vertical layout: position along Y-axis
               Container.Items(I).Position_Y := Cursor;
               Container.Items(I).Position_X := 0;
               Cursor := Cursor + Container.Items(I).Computed_Size + Gap;
            end if;
         end loop;
      end;
   end Layout;

end Flexbox;
