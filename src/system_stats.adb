with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System;
with System.Storage_Elements;

package body System_Stats is

   package C renames Interfaces.C;
   use type C.int;
   use type C.long;
   use type System.Storage_Elements.Integer_Address;

   procedure Free is new Ada.Unchecked_Deallocation (
      Object => Process_Array,
      Name   => Process_Array_Ptr
   );

   function C_Get_Platform return C.int;
   pragma Import (C, C_Get_Platform, "get_platform");

   function Get_Platform return Platform_Type is
      Platform : constant C.int := C_Get_Platform;
   begin
      case Platform is
         when 0 => return Linux;
         when 1 => return Windows;
         when others => return Unknown;
      end case;
   end Get_Platform;

   function C_Get_Num_Cores return C.int;
   pragma Import (C, C_Get_Num_Cores, "get_num_cpu_cores");

   function C_Get_CPU_Usage (Usage : access C.C_float; Max_Cores : C.int) return C.int;
   pragma Import (C, C_Get_CPU_Usage, "get_cpu_usage");

   function C_Get_Memory_Usage_Percent return C.C_float;
   pragma Import (C, C_Get_Memory_Usage_Percent, "get_memory_usage_percent");

   procedure C_Get_Memory_Detailed (
      Total_MB : access C.int;
      Used_MB : access C.int;
      Free_MB : access C.int;
      Avail_MB : access C.int;
      Buff_MB : access C.int;
      Cached_MB : access C.int;
      Swap_Total_MB : access C.int;
      Swap_Used_MB : access C.int
   );
   pragma Import (C, C_Get_Memory_Detailed, "get_memory_detailed");

   function C_Get_Disk_Usage (Path : C.Strings.chars_ptr) return C.C_float;
   pragma Import (C, C_Get_Disk_Usage, "get_disk_usage");

   procedure C_Get_Disk_IO (Read_MB : access C.C_float; Write_MB : access C.C_float);
   pragma Import (C, C_Get_Disk_IO, "get_disk_io");

   procedure C_Get_Network_IO (RX_MB : access C.C_float; TX_MB : access C.C_float);
   pragma Import (C, C_Get_Network_IO, "get_network_io");

   function C_Get_Uptime return C.long;
   pragma Import (C, C_Get_Uptime, "get_uptime_seconds");

   procedure C_Get_Load_Average (Buffer : C.Strings.chars_ptr; Buf_Size : C.int);
   pragma Import (C, C_Get_Load_Average, "get_load_average");

   function Get_CPU_Count return Natural is
   begin
      return Natural(C_Get_Num_Cores);
   end Get_CPU_Count;

   function Get_CPU_Usage (Core : Natural) return Float is
      Max_Cores : constant := 128;
      Usage_Array : array (1 .. Max_Cores) of aliased C.C_float;
      Num_Cores : C.int;
   begin
      Num_Cores := C_Get_CPU_Usage (Usage_Array(1)'Access, Max_Cores);
      if Core >= Natural(Num_Cores) then
         return 0.0;
      end if;
      return Float(Usage_Array(Core + 1));
   end Get_CPU_Usage;

   function Get_CPU_Usage_Average return Float is
      Max_Cores : constant := 128;
      Usage_Array : array (1 .. Max_Cores) of aliased C.C_float;
      Num_Cores : C.int;
      Total : Float := 0.0;
   begin
      Num_Cores := C_Get_CPU_Usage (Usage_Array(1)'Access, Max_Cores);
      if Num_Cores <= 0 then
         return 0.0;
      end if;
      for I in 1 .. Natural(Num_Cores) loop
         Total := Total + Float(Usage_Array(I));
      end loop;
      return Total / Float(Num_Cores);
   end Get_CPU_Usage_Average;

   function Get_Memory_Usage return Float is
   begin
      return Float(C_Get_Memory_Usage_Percent);
   end Get_Memory_Usage;

   procedure Get_Memory_GB (Total : out Float; Used : out Float; Available : out Float) is
      T, U, F, A, B, Ca, ST, SU : aliased C.int;
   begin
      C_Get_Memory_Detailed (T'Access, U'Access, F'Access, A'Access,
                             B'Access, Ca'Access, ST'Access, SU'Access);
      Total := Float(T) / 1024.0;
      Available := Float(A) / 1024.0;
      Used := Float(U) / 1024.0;
   end Get_Memory_GB;

   procedure Get_Memory_Detailed (
      Total     : out Natural;
      Used      : out Natural;
      Free      : out Natural;
      Available : out Natural;
      Buffers   : out Natural;
      Cached    : out Natural;
      Swap_Total : out Natural;
      Swap_Used  : out Natural
   ) is
      T, U, F, A, B, Ca, ST, SU : aliased C.int;
   begin
      C_Get_Memory_Detailed (T'Access, U'Access, F'Access, A'Access,
                             B'Access, Ca'Access, ST'Access, SU'Access);
      Total := Natural(T);
      Used := Natural(U);
      Free := Natural(F);
      Available := Natural(A);
      Buffers := Natural(B);
      Cached := Natural(Ca);
      Swap_Total := Natural(ST);
      Swap_Used := Natural(SU);
   end Get_Memory_Detailed;

   function Get_Disk_Usage (Path : String) return Float is
      C_Path : C.Strings.chars_ptr := C.Strings.New_String(Path);
      Result : C.C_float;
   begin
      Result := C_Get_Disk_Usage(C_Path);
      C.Strings.Free(C_Path);
      return Float(Result);
   end Get_Disk_Usage;

   procedure Get_Disk_IO (Read_MB : out Float; Write_MB : out Float) is
      R, W : aliased C.C_float;
   begin
      C_Get_Disk_IO (R'Access, W'Access);
      Read_MB := Float(R);
      Write_MB := Float(W);
   end Get_Disk_IO;

   procedure Get_Network_IO (RX_MB : out Float; TX_MB : out Float) is
      R, T : aliased C.C_float;
   begin
      C_Get_Network_IO (R'Access, T'Access);
      RX_MB := Float(R);
      TX_MB := Float(T);
   end Get_Network_IO;

   function Get_Uptime return Natural is
      Uptime : C.long := C_Get_Uptime;
   begin
      if Uptime < 0 then
         return 0;
      else
         return Natural(Uptime);
      end if;
   end Get_Uptime;

   function Get_Load_Average return String is
      Buffer_Size : constant := 64;
      Buffer_Arr : aliased C.char_array := C.To_C((1 .. Buffer_Size => ' '));
      Buffer : C.Strings.chars_ptr := C.Strings.To_Chars_Ptr(Buffer_Arr'Unchecked_Access);
   begin
      C_Get_Load_Average (Buffer, C.int(Buffer_Size));
      return C.Strings.Value (Buffer);
   exception
      when others =>
         return "N/A";
   end Get_Load_Average;

   type Process_Info_C is record
      PID         : C.int;
      Name        : C.char_array (0 .. 255);
      User        : C.char_array (0 .. 31);
      State       : C.int;
      CPU_Percent : C.C_float;
      Mem_Percent : C.C_float;
      Mem_KB      : C.unsigned_long;
   end record;
   pragma Convention (C, Process_Info_C);

   type Process_Info_C_Ptr is access all Process_Info_C;
   pragma Convention (C, Process_Info_C_Ptr);

   function C_Get_Process_List (Count : access C.int) return Process_Info_C_Ptr;
   pragma Import (C, C_Get_Process_List, "get_process_list");

   procedure C_Free_Process_List (List : Process_Info_C_Ptr);
   pragma Import (C, C_Free_Process_List, "free_process_list");

   function C_Kill_Process (PID : C.int) return C.int;
   pragma Import (C, C_Kill_Process, "kill_process");

   function Get_Process_List return Process_Array_Ptr is
      Count : aliased C.int := 0;
      C_List : Process_Info_C_Ptr;
      Ada_List : Process_Array_Ptr;
   begin
      C_List := C_Get_Process_List (Count'Access);
      
      if C_List = null or Count <= 0 then
         return null;
      end if;

      Ada_List := new Process_Array (1 .. Natural(Count));

      -- Access the C array using pointer arithmetic
      for I in 0 .. Natural(Count) - 1 loop
         declare
            use System.Storage_Elements;
            -- Get pointer to the I-th element
            type Proc_Ptr is access all Process_Info_C;
            function To_Proc_Ptr is new Ada.Unchecked_Conversion (
               Source => System.Address,
               Target => Proc_Ptr
            );
            Elem_Size : constant Storage_Offset := Process_Info_C'Size / System.Storage_Unit;
            Offset : constant Storage_Offset := Storage_Offset(I) * Elem_Size;
            Base_Addr : constant Integer_Address := To_Integer(C_List.all'Address);
            Offset_As_Int : constant Integer_Address := Integer_Address(Offset);
            New_Addr : constant Integer_Address := Base_Addr + Offset_As_Int;
            C_Proc_Ptr : constant Proc_Ptr := To_Proc_Ptr(To_Address(New_Addr));
         begin
            Ada_List(I + 1).PID := Natural(C_Proc_Ptr.PID);
            Ada_List(I + 1).Name := SU.To_Unbounded_String (
               C.To_Ada (C_Proc_Ptr.Name)
            );
            Ada_List(I + 1).User := SU.To_Unbounded_String (
               C.To_Ada (C_Proc_Ptr.User)
            );
            
            case C_Proc_Ptr.State is
               when 0 => Ada_List(I + 1).State := Running;
               when 1 => Ada_List(I + 1).State := Sleeping;
               when 2 => Ada_List(I + 1).State := Stopped;
               when 3 => Ada_List(I + 1).State := Zombie;
               when others => Ada_List(I + 1).State := Unknown_State;
            end case;
            
            Ada_List(I + 1).CPU := Float(C_Proc_Ptr.CPU_Percent);
            Ada_List(I + 1).Memory := Float(C_Proc_Ptr.Mem_Percent);
            Ada_List(I + 1).Mem_KB := Natural(C_Proc_Ptr.Mem_KB);
         end;
      end loop;

      C_Free_Process_List (C_List);
      return Ada_List;
   end Get_Process_List;

   procedure Free_Process_List (List : in out Process_Array_Ptr) is
   begin
      if List /= null then
         Free (List);
      end if;
   end Free_Process_List;

   function Kill_Process (PID : Natural) return Boolean is
      Result : C.int;
   begin
      Result := C_Kill_Process (C.int(PID));
      return Result /= 0;
   end Kill_Process;

end System_Stats;