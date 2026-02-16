------------------------------------------------------------------------------
--  System_Stats.ads
--
--  Single-file system statistics package for Linux.
--  C code is embedded using pragma Import.
--
--  USAGE:
--    Num_Cores := System_Stats.Get_CPU_Count;
--    Usage := System_Stats.Get_CPU_Usage (Core_Index);
--    Mem_Percent := System_Stats.Get_Memory_Usage;
--    Disk_Percent := System_Stats.Get_Disk_Usage ("/");
------------------------------------------------------------------------------

package System_Stats is

   --------------------------------------------------------
   -- CPU STATISTICS
   --------------------------------------------------------

   --  Get number of CPU cores
   function Get_CPU_Count return Natural;

   --  Get CPU usage for a specific core (0-indexed, returns 0.0 to 1.0)
   --  NOTE: First call returns 0.0, subsequent calls show actual usage
   function Get_CPU_Usage (Core : Natural) return Float;

   --  Get average CPU usage across all cores
   function Get_CPU_Usage_Average return Float;

   --------------------------------------------------------
   -- MEMORY STATISTICS
   --------------------------------------------------------

   --  Get memory usage as percentage (0.0 to 1.0)
   function Get_Memory_Usage return Float;

   --  Get memory in GB
   procedure Get_Memory_GB (Total : out Float; Used : out Float; Available : out Float);

   --------------------------------------------------------
   -- DISK STATISTICS
   --------------------------------------------------------

   --  Get disk usage for a path (returns 0.0 to 1.0)
   function Get_Disk_Usage (Path : String) return Float;

   --------------------------------------------------------
   -- SYSTEM INFO
   --------------------------------------------------------

   --  Get system uptime in seconds
   function Get_Uptime return Natural;

end System_Stats;