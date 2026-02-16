------------------------------------------------------------------------------
--  System_Stats.adb
--
--  Implementation with embedded C code for Linux system statistics.
--  All C functions are defined inline using pragma Import with Link_Name.
------------------------------------------------------------------------------

with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

package body System_Stats is

   package C renames Interfaces.C;

   --------------------------------------------------------
   -- C IMPORTS (Functions are in system_stats.c)
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
   begin
      return Natural (C_Get_Num_Cores);
   end Get_CPU_Count;

   function Get_CPU_Usage (Core : Natural) return Float is
      Max_Cores : constant := 128;
      Usage_Array : array (1 .. Max_Cores) of aliased C.C_float;
      Num_Cores : C.int;
   begin
      Num_Cores := C_Get_CPU_Usage (Usage_Array (1)'Access, Max_Cores);
      
      if Core >= Natural (Num_Cores) then
         return 0.0;
      end if;

      return Float (Usage_Array (Core + 1));
   end Get_CPU_Usage;

   function Get_CPU_Usage_Average return Float is
      Max_Cores : constant := 128;
      Usage_Array : array (1 .. Max_Cores) of aliased C.C_float;
      Num_Cores : C.int;
      Total : Float := 0.0;
   begin
      Num_Cores := C_Get_CPU_Usage (Usage_Array (1)'Access, Max_Cores);
      
      if Num_Cores = 0 then
         return 0.0;
      end if;

      for I in 1 .. Natural (Num_Cores) loop
         Total := Total + Float (Usage_Array (I));
      end loop;

      return Total / Float (Num_Cores);
   end Get_CPU_Usage_Average;

   --------------------------------------------------------
   -- MEMORY STATISTICS
   --------------------------------------------------------

   function Get_Memory_Usage return Float is
   begin
      return Float (C_Get_Memory_Usage_Percent);
   end Get_Memory_Usage;

   procedure Get_Memory_GB (Total : out Float; Used : out Float; Available : out Float) is
      Mem : aliased Memory_Stats_C;
      Result : C.int;
   begin
      Result := C_Get_Memory_Stats (Mem'Access);
      
      if Result /= 0 then
         Total := 0.0;
         Used := 0.0;
         Available := 0.0;
         return;
      end if;

      Total := Float (Mem.Total_KB) / 1_048_576.0;  -- KB to GB
      Available := Float (Mem.Available_KB) / 1_048_576.0;
      Used := Total - Available;
   end Get_Memory_GB;

   --------------------------------------------------------
   -- DISK STATISTICS
   --------------------------------------------------------

   function Get_Disk_Usage (Path : String) return Float is
      C_Path : C.Strings.chars_ptr := C.Strings.New_String (Path);
      Result : C.C_float;
   begin
      Result := C_Get_Disk_Usage (C_Path);
      C.Strings.Free (C_Path);
      return Float (Result);
   end Get_Disk_Usage;

   --------------------------------------------------------
   -- SYSTEM INFO
   --------------------------------------------------------

   function Get_Uptime return Natural is
   begin
      return Natural (C_Get_Uptime);
   end Get_Uptime;

end System_Stats;

--------------------------------------------------------------------------------
-- EMBEDDED C CODE
-- The following C code is compiled separately as system_stats.c
-- Place this in src/Linux/system_stats.c
--------------------------------------------------------------------------------

-- #include <stdio.h>
-- #include <stdlib.h>
-- #include <string.h>
-- #include <unistd.h>
-- #include <sys/sysinfo.h>
-- #include <sys/statvfs.h>
-- 
-- typedef struct {
--     unsigned long long user, nice, system, idle, iowait, irq, softirq;
-- } cpu_stat_t;
-- 
-- static cpu_stat_t prev_stats[128];
-- static int num_cores = 0;
-- static int initialized = 0;
-- 
-- static int read_cpu_stats(cpu_stat_t stats[], int max) {
--     FILE *fp = fopen("/proc/stat", "r");
--     if (!fp) return -1;
--     char line[256];
--     int count = 0;
--     while (fgets(line, sizeof(line), fp) && count < max) {
--         if (strncmp(line, "cpu", 3) == 0 && line[3] >= '0' && line[3] <= '9') {
--             sscanf(line, "cpu%*d %llu %llu %llu %llu %llu %llu %llu",
--                    &stats[count].user, &stats[count].nice, &stats[count].system,
--                    &stats[count].idle, &stats[count].iowait, &stats[count].irq,
--                    &stats[count].softirq);
--             count++;
--         }
--     }
--     fclose(fp);
--     return count;
-- }
-- 
-- static float calc_usage(cpu_stat_t *p, cpu_stat_t *c) {
--     unsigned long long p_idle = p->idle + p->iowait;
--     unsigned long long c_idle = c->idle + c->iowait;
--     unsigned long long p_total = p->user + p->nice + p->system + p_idle + p->irq + p->softirq;
--     unsigned long long c_total = c->user + c->nice + c->system + c_idle + c->irq + c->softirq;
--     unsigned long long total_diff = c_total - p_total;
--     unsigned long long idle_diff = c_idle - p_idle;
--     if (total_diff == 0) return 0.0f;
--     return (float)(total_diff - idle_diff) / (float)total_diff;
-- }
-- 
-- int get_cpu_usage(float usage[], int max) {
--     cpu_stat_t curr[128];
--     int cores = read_cpu_stats(curr, max);
--     if (cores <= 0) return 0;
--     if (!initialized) {
--         memcpy(prev_stats, curr, sizeof(cpu_stat_t) * cores);
--         num_cores = cores;
--         initialized = 1;
--         for (int i = 0; i < cores; i++) usage[i] = 0.0f;
--         return cores;
--     }
--     for (int i = 0; i < cores && i < num_cores; i++)
--         usage[i] = calc_usage(&prev_stats[i], &curr[i]);
--     memcpy(prev_stats, curr, sizeof(cpu_stat_t) * cores);
--     num_cores = cores;
--     return cores;
-- }
-- 
-- int get_num_cpu_cores(void) {
--     return sysconf(_SC_NPROCESSORS_ONLN);
-- }
-- 
-- typedef struct {
--     unsigned long total_kb, free_kb, available_kb, buffers_kb, cached_kb;
-- } memory_stat_t;
-- 
-- int get_memory_stats(memory_stat_t *mem) {
--     FILE *fp = fopen("/proc/meminfo", "r");
--     if (!fp) return -1;
--     char line[256];
--     memset(mem, 0, sizeof(memory_stat_t));
--     while (fgets(line, sizeof(line), fp)) {
--         sscanf(line, "MemTotal: %lu kB", &mem->total_kb);
--         sscanf(line, "MemFree: %lu kB", &mem->free_kb);
--         sscanf(line, "MemAvailable: %lu kB", &mem->available_kb);
--         sscanf(line, "Buffers: %lu kB", &mem->buffers_kb);
--         sscanf(line, "Cached: %lu kB", &mem->cached_kb);
--     }
--     fclose(fp);
--     return 0;
-- }
-- 
-- float get_memory_usage_percent(void) {
--     memory_stat_t mem;
--     if (get_memory_stats(&mem) != 0) return 0.0f;
--     if (mem.total_kb == 0) return 0.0f;
--     unsigned long used = mem.total_kb - mem.available_kb;
--     return (float)used / (float)mem.total_kb;
-- }
-- 
-- float get_disk_usage(const char *path) {
--     struct statvfs stat;
--     if (statvfs(path, &stat) != 0) return 0.0f;
--     unsigned long long total = stat.f_blocks * stat.f_frsize;
--     unsigned long long avail = stat.f_bavail * stat.f_frsize;
--     unsigned long long used = total - avail;
--     if (total == 0) return 0.0f;
--     return (float)used / (float)total;
-- }
-- 
-- long get_uptime_seconds(void) {
--     struct sysinfo info;
--     if (sysinfo(&info) != 0) return 0;
--     return info.uptime;
-- }