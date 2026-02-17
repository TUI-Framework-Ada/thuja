------------------------------------------------------------------------------
--  System_Stats.ads
--
--  Cross-platform system statistics package (Linux & Windows)
------------------------------------------------------------------------------
with Ada.Strings.Unbounded;

package System_Stats is

   package SU renames Ada.Strings.Unbounded;

   --------------------------------------------------------
   -- PLATFORM DETECTION
   --------------------------------------------------------
   
   type Platform_Type is (Linux, Windows, Unknown);
   
   function Get_Platform return Platform_Type;

   --------------------------------------------------------
   -- CPU STATISTICS
   --------------------------------------------------------
   
   function Get_CPU_Count return Natural;
   function Get_CPU_Usage (Core : Natural) return Float;
   function Get_CPU_Usage_Average return Float;

   --------------------------------------------------------
   -- MEMORY STATISTICS
   --------------------------------------------------------
   
   function Get_Memory_Usage return Float;
   procedure Get_Memory_GB (Total : out Float; Used : out Float; Available : out Float);
   
   procedure Get_Memory_Detailed (
      Total     : out Natural;
      Used      : out Natural;
      Free      : out Natural;
      Available : out Natural;
      Buffers   : out Natural;
      Cached    : out Natural;
      Swap_Total : out Natural;
      Swap_Used  : out Natural
   );

   --------------------------------------------------------
   -- DISK STATISTICS
   --------------------------------------------------------
   
   function Get_Disk_Usage (Path : String) return Float;
   
   procedure Get_Disk_IO (
      Read_MB  : out Float;
      Write_MB : out Float
   );

   --------------------------------------------------------
   -- NETWORK STATISTICS
   --------------------------------------------------------
   
   procedure Get_Network_IO (
      RX_MB : out Float;
      TX_MB : out Float
   );

   --------------------------------------------------------
   -- SYSTEM INFO
   --------------------------------------------------------
   
   function Get_Uptime return Natural;
   function Get_Load_Average return String;
   
   --------------------------------------------------------
   -- PROCESS INFORMATION
   --------------------------------------------------------
   
   type Process_State is (Running, Sleeping, Stopped, Zombie, Unknown_State);
   
   type Process_Info is record
      PID      : Natural;
      Name     : SU.Unbounded_String;
      User     : SU.Unbounded_String;
      CPU      : Float;
      Memory   : Float;
      Mem_KB   : Natural;
      State    : Process_State;
   end record;
   
   type Process_Array is array (Positive range <>) of Process_Info;
   type Process_Array_Ptr is access Process_Array;
   
   function Get_Process_List return Process_Array_Ptr;
   procedure Free_Process_List (List : in out Process_Array_Ptr);
   function Kill_Process (PID : Natural) return Boolean;

end System_Stats;