package body htop is

   ----------------------------------------------------------------------------
   --  Internal helpers
   ----------------------------------------------------------------------------

   function Img (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Img;

   function Img_F1 (F : Float) return String is
      W : constant Natural := Natural (Float'Floor (F));
      D : constant Natural :=
        Natural (Float'Floor ((F - Float'Floor (F)) * 10.0));
   begin
      return Img (W) & "." & Img (D);
   end Img_F1;

   function Pad (S : String; Len : Natural) return String is
      Result   : String (1 .. Len) := [others => ' '];
      Copy_Len : constant Natural := Natural'Min (S'Length, Len);
   begin
      Result (1 .. Copy_Len) := S (S'First .. S'First + Copy_Len - 1);
      return Result;
   end Pad;

   ----------------------------------------------------------------------------
   --  Initialise
   ----------------------------------------------------------------------------
   procedure Initialise is
      Dummy_Count : Natural;
      Dummy       : Float;
   begin
      if SS.Get_Platform = SS.Windows then
         Disk_Path := SU.To_Unbounded_String ("C:\");
      else
         Disk_Path := SU.To_Unbounded_String ("/");
      end if;

      Dummy := SS.Get_CPU_Usage_Average;
      SS.Get_All_CPU_Usages (CPU_Values, Dummy_Count);
      Num_Cores := Natural'Min (Dummy_Count, Max_Cores);
   end Initialise;

   ----------------------------------------------------------------------------
   --  Refresh
   ----------------------------------------------------------------------------
   procedure Refresh is
      Core_Count                  : Natural;
      Tot_MB,
      Used_MB,
      Free_MB,
      Avail_MB,
      Buff_MB,
      Cache_MB,
      Swap_Tot_MB,
      Swap_Used_MB                : Natural;
      Disk_Total_GB, Disk_Used_GB : Float;
      Procs                       : SS.Process_Array_Ptr;
      Path                        : constant String :=
        SU.To_String (Disk_Path);
   begin
      --------------------------------------------------------
      --  CPU
      --------------------------------------------------------
      declare
         New_Values : SS.CPU_Usage_Array := [others => 0.0];
      begin
         SS.Get_All_CPU_Usages (New_Values, Core_Count);
         Num_Cores := Natural'Min (Core_Count, Max_Cores);
         for C in 0 .. Num_Cores - 1 loop
            CPU_Values (C) := CPU_Values (C) * 0.7 + New_Values (C) * 0.3;
         end loop;
      end;
      --------------------------------------------------------
      --  Memory
      --------------------------------------------------------
      SS.Get_Memory_Detailed
        (Tot_MB,
         Used_MB,
         Free_MB,
         Avail_MB,
         Buff_MB,
         Cache_MB,
         Swap_Tot_MB,
         Swap_Used_MB);
      declare
         Real_Used : constant Natural := Tot_MB - Free_MB - Buff_MB - Cache_MB;
      begin
         Mem_Pct :=
           (if Tot_MB > 0 then Float (Real_Used) / Float (Tot_MB) else 0.0);
         Swap_Pct :=
           (if Swap_Tot_MB > 0
            then Float (Swap_Used_MB) / Float (Swap_Tot_MB)
            else 0.0);
         Mem_Label :=
           SU.To_Unbounded_String
             ("Memory: "
              & Img_F1 (Float (Real_Used) / 1024.0)
              & "G / "
              & Img_F1 (Float (Tot_MB) / 1024.0)
              & "G");
      end;

      --------------------------------------------------------
      --  Disk
      --------------------------------------------------------
      SS.Get_Disk_Space_GB (Path, Disk_Total_GB, Disk_Used_GB);
      Disk_Pct := SS.Get_Disk_Usage (Path);
      Disk_Label :=
        SU.To_Unbounded_String
          ("Disk: "
           & Img_F1 (Disk_Used_GB)
           & "G / "
           & Img_F1 (Disk_Total_GB)
           & "G");

      --------------------------------------------------------
      --  Process table
      --------------------------------------------------------
      Procs := SS.Get_Process_List;

      for R in 0 .. Num_Proc_Rows - 1 loop
         if Procs /= null and then R < Procs'Length then
            declare
               P         : SS.Process_Info renames Procs (R + 1);
               CPU_Pct   : constant Natural :=
                 Natural'Min (100, Natural (P.CPU));
               Mem_Pct_N : constant Natural :=
                 Natural'Min (100, Natural (P.Memory));
               St        : constant Character :=
                 (case P.State is
                    when SS.Running       => 'R',
                    when SS.Sleeping      => 'S',
                    when SS.Stopped       => 'T',
                    when SS.Zombie        => 'Z',
                    when SS.Unknown_State => '?');
            begin
               Proc_Rows (R).Text :=
                 SU.To_Unbounded_String
                   (Pad (Img (P.PID), 7)
                    & Pad (SU.To_String (P.User), 10)
                    & Pad (Img (CPU_Pct) & "%", 6)
                    & Pad (Img (Mem_Pct_N) & "%", 6)
                    & St
                    & "  "
                    & Pad (SU.To_String (P.Name), 30));
               Proc_Rows (R).High_CPU := CPU_Pct > 50;
               Proc_Rows (R).Mid_CPU := CPU_Pct > 20;
            end;
         else
            Proc_Rows (R).Text := SU.Null_Unbounded_String;
            Proc_Rows (R).High_CPU := False;
            Proc_Rows (R).Mid_CPU := False;
         end if;
      end loop;

      if Procs /= null then
         SS.Free_Process_List (Procs);
      end if;
   end Refresh;

end htop;