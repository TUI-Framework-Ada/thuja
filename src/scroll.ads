package Scroll is

   ----------------------------------------------------------------------------
   --  Scroll
   --
   --  PURPOSE
   --  -------
   --  Provides a reusable vertical scroll viewport calculation for any
   --  widget that needs to display a subset of lines from a larger buffer.
   --  This package is intentionally decoupled from any specific widget or
   --  buffer type so it can be used by any demo or widget in the framework.
   --
   --  USAGE
   --  -----
   --  The caller owns all state (Scroll_Offset, Visible_Rows etc.) and
   --  passes it in as parameters. This package performs the calculation
   --  and updates Scroll_Offset in place via an out parameter.
   --
   --  EXAMPLE
   --  -------
   --  Scroll.Update
   --    (Current_Line  => Text_Editor.Current_Line,
   --     Total_Lines   => Natural (Text_Editor.Lines.Length),
   --     Visible_Rows  => 22,
   --     Scroll_Offset => My_Scroll_Offset);
   ----------------------------------------------------------------------------

   --------------------------------------------------------
   -- Update
   -- Adjusts Scroll_Offset so that Current_Line always
   -- falls within the visible window defined by
   -- Scroll_Offset and Visible_Rows.
   --
   -- Rules:
   --   - If Current_Line is above Scroll_Offset, scroll up
   --     so Current_Line is the top visible line
   --   - If Current_Line is below the bottom of the window,
   --     scroll down so Current_Line is the bottom visible line
   --   - Scroll_Offset is always clamped so it never exceeds
   --     Total_Lines - Visible_Rows
   --   - Scroll_Offset never goes below 0
   --------------------------------------------------------
   procedure Update
      (Current_Line  : in     Natural;
       Total_Lines   : in     Natural;
       Visible_Rows  : in     Positive;
       Scroll_Offset : in out Natural);

end Scroll;