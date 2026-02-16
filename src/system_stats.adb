-------------------------------------------------------------------------------
--  System_Stats.adb
--
--  Implementation with embedded C code for Linux system statistics.
-------------------------------------------------------------------------------

with Interfaces.C;
with Interfaces.C.Strings;

package body System_Stats is

   package C renames Interfaces.C;

   --------------------------------------------------------
   -- C IMPORTS
   --------------------------------------------------------

   function C_Get_Num_Cores return C.int;
   pragma Import (C, C_Get_Num_Cores, "get_num_cpu_cores");

   function C_Get_CPU_Usage (Usage : access C.C_float; Max_Cores : C.int) return C.int;
   pragma Import (C, C_Get_CPU_Usage, "get_cpu_usage");

   function C_Get_Memory_Usage_Percent return C.C_float;
   pragma Import (C, C_Get_Memory_Usage_Percent, "get_memory_usage_percent");

   type Memory_Stats_C is record
      Total_KB     : C.unsigned_long;
      Free_KB      : C.unsigned_long;
      Available_KB : C.unsigned_long;
      Buffers_KB   : C.unsigned_long;
      Cached_KB    : C.unsigned_long;
   end record;
   pragma Convention (C, Memory_Stats_C);

   function C_Get_Memory_Stats (Mem : access Memory_Stats_C) return C.int;
   pragma Import (C, C_Get_Memory_Stats, "get_memory_stats");

   function C_Get_Disk_Usage (Path : C.Strings.chars_ptr) return C.C_float;
   pragma Import (C, C_Get_Disk_Usage, "get_disk_usage");

   function C_Get_Uptime return C.long;
   pragma Import (C, C_Get_Uptime, "get_uptime_seconds");

   --------------------------------------------------------
   -- CPU STATISTICS
   --------------------------------------------------------

   function Get_CPU_Count return Natural is
      Num : C.int := C_Get_Num_Cores;
   begin
      return Natural(Integer(Num));
   end Get_CPU_Count;

   function Get_CPU_Usage (Core : Natural) return Float is
      Max_Cores : constant := 128;
      Usage_Array : array (1 .. Max_Cores) of aliased C.C_float;
      Num_Cores : C.int;
      Num_Cores_Ad : Integer;
   begin
      Num_Cores := C_Get_CPU_Usage (Usage_Array(1)'Access, Max_Cores);
      Num_Cores_Ad := Integer(Num_Cores);

      if Core >= Natural(Num_Cores_Ad) then
         return 0.0;
      end if;

      return Float(Usage_Array(Core + 1));
   end Get_CPU_Usage;

   function Get_CPU_Usage_Average return Float is
      Max_Cores : constant := 128;
      Usage_Array : array (1 .. Max_Cores) of aliased C.C_float;
      Num_Cores : C.int;
      Num_Cores_Ad : Integer;
      Total : Float := 0.0;
   begin
      Num_Cores := C_Get_CPU_Usage (Usage_Array(1)'Access, Max_Cores);
      Num_Cores_Ad := Integer(Num_Cores);

      if Num_Cores_Ad <= 0 then
         return 0.0;
      end if;

      for I in 1 .. Num_Cores_Ad loop
         Total := Total + Float(Usage_Array(I));
      end loop;

      return Total / Float(Num_Cores_Ad);
   end Get_CPU_Usage_Average;

   --------------------------------------------------------
   -- MEMORY STATISTICS
   --------------------------------------------------------

   function Get_Memory_Usage return Float is
   begin
      return Float(C_Get_Memory_Usage_Percent);
   end Get_Memory_Usage;

   procedure Get_Memory_GB (Total : out Float; Used : out Float; Available : out Float) is
      Mem : aliased Memory_Stats_C;
      Result : C.int;
      Result_Ad : Integer;
   begin
      Result := C_Get_Memory_Stats(Mem'Access);
      Result_Ad := Integer(Result);

      if Result_Ad /= 0 then
         Total := 0.0;
         Used := 0.0;
         Available := 0.0;
         return;
      end if;

      Total := Float(Mem.Total_KB) / 1_048_576.0;      -- KB to GB
      Available := Float(Mem.Available_KB) / 1_048_576.0;
      Used := Total - Available;
   end Get_Memory_GB;

   --------------------------------------------------------
   -- DISK STATISTICS
   --------------------------------------------------------

   function Get_Disk_Usage (Path : String) return Float is
      C_Path : C.Strings.chars_ptr := C.Strings.New_String(Path);
      Result : C.C_float;
   begin
      Result := C_Get_Disk_Usage(C_Path);
      C.Strings.Free(C_Path);
      return Float(Result);
   end Get_Disk_Usage;

   --------------------------------------------------------
   -- SYSTEM INFO
   --------------------------------------------------------

   function Get_Uptime return Natural is
      Uptime : C.long := C_Get_Uptime;
      Uptime_Ad : Long_Integer := Long_Integer(Uptime);
   begin
      if Uptime_Ad < 0 then
         return 0;
      else
         return Natural(Uptime_Ad);
      end if;
   end Get_Uptime;

end System_Stats;
