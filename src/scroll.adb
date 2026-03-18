package body Scroll is

   ----------------------------------------------------------------------------
   --  Scroll.adb
   --
   --  PURPOSE
   --  -------
   --  Implements the vertical scroll viewport calculation. Adjusts
   --  Scroll_Offset so that Current_Line is always within the visible
   --  window. This logic is intentionally simple and stateless so it
   --  can be used by any widget in the framework.
   ----------------------------------------------------------------------------

   procedure Update
      (Current_Line  : in     Natural;
       Total_Lines   : in     Natural;
       Visible_Rows  : in     Positive;
       Scroll_Offset : in out Natural)
   is
      Bottom_Of_Window : constant Natural :=
         Scroll_Offset + Visible_Rows - 1;
   begin
      --  If the cursor has moved above the visible window,
      --  scroll up so Current_Line is the top visible line
      if Current_Line < Scroll_Offset then
         Scroll_Offset := Current_Line;

      --  If the cursor has moved below the visible window,
      --  scroll down so Current_Line is the bottom visible line
      elsif Current_Line > Bottom_Of_Window then
         Scroll_Offset := Current_Line - Visible_Rows + 1;
      end if;

      --  Clamp Scroll_Offset so we never scroll past the last
      --  page of content. Only applies when buffer is larger
      --  than the visible area.
      if Total_Lines > Visible_Rows then
         if Scroll_Offset > Total_Lines - Visible_Rows then
            Scroll_Offset := Total_Lines - Visible_Rows;
         end if;
      else
         --  Buffer fits entirely in the visible area,
         --  no scrolling needed
         Scroll_Offset := 0;
      end if;
   end Update;

end Scroll;