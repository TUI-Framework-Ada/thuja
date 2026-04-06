package body Flexbox is

   procedure Layout (Container : in out Flex_Container) is
      Total_Basis  : Integer := 0;
      Total_Grow   : Float := 0.0;
      Total_Shrink : Float := 0.0;
      Free_Space   : Integer := 0;
   begin
      if Container.Items = null or Container.Item_Count = 0 then
         return;
      end if;

      -------------------------------------------------------------------------
      -- Step 1: Sum basis and grow/shrink factors
      -------------------------------------------------------------------------
      for I in 1 .. Container.Item_Count loop
         Total_Basis := Total_Basis + Container.Items (I).Flex_Basis;
         Total_Grow := Total_Grow + Container.Items (I).Flex_Grow;
         Total_Shrink := Total_Shrink + Container.Items (I).Flex_Shrink;
      end loop;

      -------------------------------------------------------------------------
      -- Step 2: Calculate free space along the main axis
      -------------------------------------------------------------------------
      if Container.Direction = Row then
         Free_Space := Container.Width - Total_Basis;
      else
         Free_Space := Container.Height - Total_Basis;
      end if;

      -------------------------------------------------------------------------
      -- Step 3: Distribute free space (grow / shrink / no change)
      -------------------------------------------------------------------------
      for I in 1 .. Container.Item_Count loop
         if Free_Space > 0 and Total_Grow > 0.0 then
            Container.Items (I).Computed_Size :=
              Container.Items (I).Flex_Basis
              + Integer
                  (Float (Free_Space)
                   * (Container.Items (I).Flex_Grow / Total_Grow));
         elsif Free_Space < 0 and Total_Shrink > 0.0 then
            Container.Items (I).Computed_Size :=
              Integer'Max
                (1,
                 Container.Items (I).Flex_Basis
                 + Integer
                     (Float (Free_Space)
                      * (Container.Items (I).Flex_Shrink / Total_Shrink)));
         else
            Container.Items (I).Computed_Size :=
              Container.Items (I).Flex_Basis;
         end if;
      end loop;

      -------------------------------------------------------------------------
      -- Step 4: Set cross-axis size
      --  Stretch    -> full container cross dimension
      --  Flex_Start -> half container cross dimension (short box, sits at top)
      --  Center     -> half container cross dimension (short box, centered)
      -------------------------------------------------------------------------
      for I in 1 .. Container.Item_Count loop
         case Container.Align is
            when Stretch             =>
               Container.Items (I).Cross_Size :=
                 (if Container.Direction = Row
                  then Container.Height
                  else Container.Width);

            when Flex_Start | Center =>
               Container.Items (I).Cross_Size :=
                 (if Container.Direction = Row
                  then Container.Height / 2
                  else Container.Width / 2);
         end case;
      end loop;

      -------------------------------------------------------------------------
      -- Step 5: Determine positions along main axis and cross axis
      -------------------------------------------------------------------------
      declare
         Cursor : Integer := 0;
         Gap    : Integer := 0;
      begin
         if Total_Grow = 0.0 then
            case Container.Justify is
               when Center        =>
                  Cursor := Free_Space / 2;

               when Space_Between =>
                  if Container.Item_Count > 1 then
                     Gap := Free_Space / (Container.Item_Count - 1);
                  end if;

               when Flex_Start    =>
                  null;
            end case;
         end if;

         for I in 1 .. Container.Item_Count loop
            if Container.Direction = Row then
               Container.Items (I).Position_X := Cursor;
               case Container.Align is
                  when Flex_Start | Stretch =>
                     Container.Items (I).Position_Y := 0;

                  when Center               =>
                     Container.Items (I).Position_Y :=
                       (Container.Height - Container.Items (I).Cross_Size) / 2;
               end case;
               Cursor := Cursor + Container.Items (I).Computed_Size + Gap;
            else
               Container.Items (I).Position_Y := Cursor;
               case Container.Align is
                  when Flex_Start | Stretch =>
                     Container.Items (I).Position_X := 0;

                  when Center               =>
                     Container.Items (I).Position_X :=
                       (Container.Width - Container.Items (I).Cross_Size) / 2;
               end case;
               Cursor := Cursor + Container.Items (I).Computed_Size + Gap;
            end if;
         end loop;
      end;

      -------------------------------------------------------------------------
      -- Step 6: Rounding adjustment — only when items were growing
      -------------------------------------------------------------------------
      if Total_Grow > 0.0 then
         declare
            Used_Space   : Integer := 0;
            Target_Space : constant Integer :=
              (if Container.Direction = Row
               then Container.Width
               else Container.Height);
         begin
            for I in 1 .. Container.Item_Count loop
               Used_Space := Used_Space + Container.Items (I).Computed_Size;
            end loop;
            if Used_Space /= Target_Space then
               Container.Items (Container.Item_Count).Computed_Size :=
                 Container.Items (Container.Item_Count).Computed_Size
                 + (Target_Space - Used_Space);
            end if;
         end;
      end if;

   end Layout;

end Flexbox;