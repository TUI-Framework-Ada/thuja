with System_Stats;
with Ada.Strings.Unbounded;

package htop is

   ----------------------------------------------------------------------------
   --  htop
   --
   --  Owns all data-fetching and formatting logic for the HTop tab.
   --  Follows the same pattern as Text_Editor and Flex_Demo: pure data,
   --  no ECS or rendering imports. Tab_Demo only does the ECS wiring.
   --
   --  Call Initialise once at startup (primes the CPU delta baseline).
   --  Call Refresh each frame to pull fresh values from System_Stats.
   --  Read the typed fields directly to populate ECS components.
   ----------------------------------------------------------------------------

   package SU renames Ada.Strings.Unbounded;
   package SS renames System_Stats;
   use type SS.Platform_Type;
   use type SS.Process_Array_Ptr;

   --  Maximum cores we display (hardware may have more)
   Max_Cores : constant := 8;

   --  Number of process rows shown in the table
   Num_Proc_Rows : constant := 10;

   --------------------------------------------------------
   --  CPU
   --------------------------------------------------------
   Num_Cores  : Natural := 0;
   CPU_Values : SS.CPU_Usage_Array := [others => 0.0];

   --------------------------------------------------------
   --  Memory
   --------------------------------------------------------
   Mem_Label : SU.Unbounded_String := SU.Null_Unbounded_String;
   Mem_Pct   : Float := 0.0;
   Swap_Pct  : Float := 0.0;

   --------------------------------------------------------
   --  Disk
   --------------------------------------------------------
   Disk_Label : SU.Unbounded_String := SU.Null_Unbounded_String;
   Disk_Pct   : Float := 0.0;

   --------------------------------------------------------
   --  Process table
   --------------------------------------------------------
   type Proc_Row is record
      Text     : SU.Unbounded_String;
      High_CPU : Boolean := False;  -- CPU > 50 %  → red
      Mid_CPU  : Boolean := False;  -- CPU > 20 %  → gold
   end record;

   type Proc_Row_Array is array (0 .. Num_Proc_Rows - 1) of Proc_Row;
   Proc_Rows : Proc_Row_Array;

   --------------------------------------------------------
   --  Disk path (set once at startup based on platform)
   --------------------------------------------------------
   Disk_Path : SU.Unbounded_String := SU.Null_Unbounded_String;

   --  Must be called once before the first Refresh.
   --  Primes the CPU delta baseline and sets Disk_Path.
   procedure Initialise;

   --  Fetch fresh values from System_Stats and update all fields above.
   --  Call once per frame while the HTop tab is active.
   procedure Refresh;

end htop;
