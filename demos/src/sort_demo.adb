with Ada.Numerics.Discrete_Random;
with Ada.Real_Time;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

use Ada.Strings.Unbounded;
use type Ada.Real_Time.Time;
use type Ada.Real_Time.Time_Span;

package body Sort_Demo is

   ---------------------------------------------------------------
   --  CONSTANTS
   ---------------------------------------------------------------

   Num_Bars  : constant Positive := 40;

   ---------------------------------------------------------------
   --  TYPES
   ---------------------------------------------------------------

   type Step_Action_T is (Compare, Swap, Set_Value,
                          Mark_Sorted, Mark_All_Sorted,
                          Mark_Deleted);

   type Sort_Step_T is record
      Action    : Step_Action_T := Compare;
      Index_A   : Natural       := 0;
      Index_B   : Natural       := 0;
      Value     : Natural       := 0;
      Code_Line : Natural       := 0;
   end record;

   package Step_Vectors is new Ada.Containers.Vectors
      (Index_Type => Natural, Element_Type => Sort_Step_T);

   type Sort_Algorithm_T is
      (Bubble_Sort, Insertion_Sort, Selection_Sort,
       Quick_Sort, Merge_Sort, Stalin_Sort);

   type Bar_State_T is (Unsorted, Comparing, Swapping, Sorted, Deleted);

   type Bar_Array_T       is array (1 .. Num_Bars) of Positive;
   type Bar_State_Array_T is array (1 .. Num_Bars) of Bar_State_T;

   Max_Source_Lines : constant := 30;
   type Source_Lines_T is array (1 .. Max_Source_Lines) of Unbounded_String;

   type Source_Code_T is record
      Lines : Source_Lines_T := (others => Null_Unbounded_String);
      Count : Natural        := 0;
   end record;

   ---------------------------------------------------------------
   --  COLOUR PALETTE
   ---------------------------------------------------------------

   Color_Bar_BG     : constant Color_t := Black;
   Color_Bar_Normal : constant Color_t := Gray;
   Color_Bar_Cmp    : constant Color_t := Yellow;
   Color_Bar_Swap   : constant Color_t := Red;
   Color_Bar_Done   : constant Color_t := Green;
   Color_Code_BG    : constant Color_t := (20, 20, 50);
   Color_Code_HL    : constant Color_t := (80, 80, 0);
   Color_Code_Txt   : constant Color_t := White;
   Color_Code_Hdr   : constant Color_t := Cyan;
   Color_Status_BG  : constant Color_t := (30, 30, 60);

   ---------------------------------------------------------------
   --  STATE
   ---------------------------------------------------------------

   Bars       : Bar_Array_T;
   Bar_States : Bar_State_Array_T := (others => Unsorted);

   Steps        : Step_Vectors.Vector;
   Current_Step : Natural := 0;
   Is_Paused    : Boolean := True;
   Is_Finished  : Boolean := False;
   Current_Algo : Sort_Algorithm_T := Bubble_Sort;
   Delay_MS     : Natural := 200;
   Active_Line  : Natural := 0;

   Last_Step_Time : Ada.Real_Time.Time := Ada.Real_Time.Clock;

   Bubble_Source    : Source_Code_T;
   Insertion_Source : Source_Code_T;
   Selection_Source : Source_Code_T;
   Quick_Source     : Source_Code_T;
   Merge_Source     : Source_Code_T;
   Stalin_Source    : Source_Code_T;

   ---------------------------------------------------------------
   --  RANDOM NUMBER GENERATOR
   ---------------------------------------------------------------

   subtype Rand_Range_T is Positive range 1 .. Num_Bars;
   package Rand_Pkg is new Ada.Numerics.Discrete_Random (Rand_Range_T);
   Gen : Rand_Pkg.Generator;

   ---------------------------------------------------------------
   --  SMALL HELPERS
   ---------------------------------------------------------------

   function Img (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Img;

   function Algo_Name (A : Sort_Algorithm_T) return String is
   begin
      case A is
         when Bubble_Sort    => return "Bubble Sort";
         when Insertion_Sort => return "Insertion Sort";
         when Selection_Sort => return "Selection Sort";
         when Quick_Sort     => return "Quick Sort";
         when Merge_Sort     => return "Merge Sort";
         when Stalin_Sort    => return "Stalin Sort";
      end case;
   end Algo_Name;

   function Get_Source return Source_Code_T is
   begin
      case Current_Algo is
         when Bubble_Sort    => return Bubble_Source;
         when Insertion_Sort => return Insertion_Source;
         when Selection_Sort => return Selection_Source;
         when Quick_Sort     => return Quick_Source;
         when Merge_Sort     => return Merge_Source;
         when Stalin_Sort    => return Stalin_Source;
      end case;
   end Get_Source;

   function State_Color (S : Bar_State_T) return Color_t is
   begin
      case S is
         when Unsorted  => return Color_Bar_Normal;
         when Comparing => return Color_Bar_Cmp;
         when Swapping  => return Color_Bar_Swap;
         when Sorted    => return Color_Bar_Done;
         when Deleted   => return Color_Bar_BG;
      end case;
   end State_Color;

   ---------------------------------------------------------------
   --  SOURCE CODE INITIALISATION
   ---------------------------------------------------------------

   procedure AL (Src : in out Source_Code_T; L : String) is
   begin
      Src.Count := Src.Count + 1;
      Src.Lines (Src.Count) := To_Unbounded_String (L);
   end AL;

   procedure Init_Source is
   begin
      AL (Bubble_Source, "procedure Bubble_Sort is");
      AL (Bubble_Source, "   Tmp : Positive;");
      AL (Bubble_Source, "begin");
      AL (Bubble_Source, "   for I in reverse 2..N loop");
      AL (Bubble_Source, "      for J in 1..I-1 loop");
      AL (Bubble_Source, "         if A(J) > A(J+1) then");
      AL (Bubble_Source, "            Tmp := A(J);");
      AL (Bubble_Source, "            A(J) := A(J+1);");
      AL (Bubble_Source, "            A(J+1) := Tmp;");
      AL (Bubble_Source, "         end if;");
      AL (Bubble_Source, "      end loop;");
      AL (Bubble_Source, "   end loop;");
      AL (Bubble_Source, "end Bubble_Sort;");

      AL (Insertion_Source, "procedure Insertion_Sort is");
      AL (Insertion_Source, "   Key : Positive;");
      AL (Insertion_Source, "   J   : Natural;");
      AL (Insertion_Source, "begin");
      AL (Insertion_Source, "   for I in 2..N loop");
      AL (Insertion_Source, "      Key := A(I);");
      AL (Insertion_Source, "      J := I - 1;");
      AL (Insertion_Source, "      while J >= 1");
      AL (Insertion_Source, "        and then A(J) > Key");
      AL (Insertion_Source, "      loop");
      AL (Insertion_Source, "         A(J+1) := A(J);");
      AL (Insertion_Source, "         J := J - 1;");
      AL (Insertion_Source, "      end loop;");
      AL (Insertion_Source, "      A(J+1) := Key;");
      AL (Insertion_Source, "   end loop;");
      AL (Insertion_Source, "end Insertion_Sort;");

      AL (Selection_Source, "procedure Selection_Sort is");
      AL (Selection_Source, "   Min, Tmp : Positive;");
      AL (Selection_Source, "begin");
      AL (Selection_Source, "   for I in 1..N-1 loop");
      AL (Selection_Source, "      Min := I;");
      AL (Selection_Source, "      for J in I+1..N loop");
      AL (Selection_Source, "         if A(J) < A(Min) then");
      AL (Selection_Source, "            Min := J;");
      AL (Selection_Source, "         end if;");
      AL (Selection_Source, "      end loop;");
      AL (Selection_Source, "      Tmp := A(I);");
      AL (Selection_Source, "      A(I) := A(Min);");
      AL (Selection_Source, "      A(Min) := Tmp;");
      AL (Selection_Source, "   end loop;");
      AL (Selection_Source, "end Selection_Sort;");

      AL (Quick_Source, "procedure Quick_Sort(Lo,Hi) is");
      AL (Quick_Source, "   Pivot, P : Integer;");
      AL (Quick_Source, "begin");
      AL (Quick_Source, "   if Lo < Hi then");
      AL (Quick_Source, "      Pivot := A(Hi);");
      AL (Quick_Source, "      P := Lo - 1;");
      AL (Quick_Source, "      for J in Lo..Hi-1 loop");
      AL (Quick_Source, "         if A(J) <= Pivot then");
      AL (Quick_Source, "            P := P + 1;");
      AL (Quick_Source, "            Swap A(P), A(J);");
      AL (Quick_Source, "         end if;");
      AL (Quick_Source, "      end loop;");
      AL (Quick_Source, "      Swap A(P+1), A(Hi);");
      AL (Quick_Source, "      Quick_Sort(Lo, P);");
      AL (Quick_Source, "      Quick_Sort(P+2, Hi);");
      AL (Quick_Source, "   end if;");
      AL (Quick_Source, "end Quick_Sort;");

      AL (Merge_Source, "procedure Merge_Sort(Lo,Hi) is");
      AL (Merge_Source, "   Mid : Integer;");
      AL (Merge_Source, "begin");
      AL (Merge_Source, "   if Lo < Hi then");
      AL (Merge_Source, "      Mid := (Lo+Hi)/2;");
      AL (Merge_Source, "      Merge_Sort(Lo, Mid);");
      AL (Merge_Source, "      Merge_Sort(Mid+1, Hi);");
      AL (Merge_Source, "      -- Merge halves:");
      AL (Merge_Source, "      L := copy A(Lo..Mid);");
      AL (Merge_Source, "      R := copy A(Mid+1..Hi);");
      AL (Merge_Source, "      while L and R remain loop");
      AL (Merge_Source, "         if L(i) <= R(j) then");
      AL (Merge_Source, "            A(k) := L(i); i++;");
      AL (Merge_Source, "         else");
      AL (Merge_Source, "            A(k) := R(j); j++;");
      AL (Merge_Source, "         end if;");
      AL (Merge_Source, "      end loop;");
      AL (Merge_Source, "      copy remaining;");
      AL (Merge_Source, "   end if;");
      AL (Merge_Source, "end Merge_Sort;");

      AL (Stalin_Source, "procedure Stalin_Sort is");
      AL (Stalin_Source, "   Max : Positive;");
      AL (Stalin_Source, "begin");
      AL (Stalin_Source, "   Max := A(1);");
      AL (Stalin_Source, "   for I in 2..N loop");
      AL (Stalin_Source, "      if A(I) >= Max then");
      AL (Stalin_Source, "         Max := A(I);");
      AL (Stalin_Source, "      else");
      AL (Stalin_Source, "         -- To the gulag!");
      AL (Stalin_Source, "         Delete A(I);");
      AL (Stalin_Source, "      end if;");
      AL (Stalin_Source, "   end loop;");
      AL (Stalin_Source, "end Stalin_Sort;");
   end Init_Source;

   ---------------------------------------------------------------
   --  SHUFFLE
   ---------------------------------------------------------------

   procedure Shuffle_Array is
      J, Tmp : Positive;
   begin
      for I in 1 .. Num_Bars loop
         Bars (I) := I;
      end loop;
      Rand_Pkg.Reset (Gen);
      for I in reverse 2 .. Num_Bars loop
         J := ((Rand_Pkg.Random (Gen) - 1) mod I) + 1;
         Tmp := Bars (I);
         Bars (I) := Bars (J);
         Bars (J) := Tmp;
      end loop;
      Bar_States := (others => Unsorted);
   end Shuffle_Array;

   ---------------------------------------------------------------
   --  STEP RECORDING
   ---------------------------------------------------------------

   procedure Record_Bubble is
      A   : Bar_Array_T := Bars;
      Tmp : Positive;
   begin
      Steps.Clear;
      for I in reverse 2 .. Num_Bars loop
         for J in 1 .. I - 1 loop
            Steps.Append (Sort_Step_T'(Compare, J, J + 1, 0, 6));
            if A (J) > A (J + 1) then
               Steps.Append (Sort_Step_T'(Compare, J, J + 1, 0, 7));
               Steps.Append (Sort_Step_T'(Swap, J, J + 1, 0, 8));
               Steps.Append (Sort_Step_T'(Compare, J, J + 1, 0, 9));
               Tmp := A (J); A (J) := A (J + 1); A (J + 1) := Tmp;
            end if;
         end loop;
         Steps.Append (Sort_Step_T'(Mark_Sorted, I, 0, 0, 12));
      end loop;
      Steps.Append (Sort_Step_T'(Mark_Sorted, 1, 0, 0, 13));
      Steps.Append (Sort_Step_T'(Mark_All_Sorted, 0, 0, 0, 13));
   end Record_Bubble;

   procedure Record_Insertion is
      A   : Bar_Array_T := Bars;
      Key : Positive;
      J   : Natural;
   begin
      Steps.Clear;
      for I in 2 .. Num_Bars loop
         Key := A (I);
         J := I - 1;
         Steps.Append (Sort_Step_T'(Compare, I, 0, 0, 6));
         while J >= 1 and then A (J) > Key loop
            Steps.Append (Sort_Step_T'(Compare, J, J + 1, 0, 9));
            Steps.Append (Sort_Step_T'(Set_Value, J + 1, J, A (J), 11));
            A (J + 1) := A (J);
            J := J - 1;
         end loop;
         Steps.Append (Sort_Step_T'(Set_Value, J + 1, 0, Key, 14));
         A (J + 1) := Key;
      end loop;
      Steps.Append (Sort_Step_T'(Mark_All_Sorted, 0, 0, 0, 16));
   end Record_Insertion;

   procedure Record_Selection is
      A   : Bar_Array_T := Bars;
      Min : Positive;
      Tmp : Positive;
   begin
      Steps.Clear;
      for I in 1 .. Num_Bars - 1 loop
         Min := I;
         Steps.Append (Sort_Step_T'(Compare, I, 0, 0, 5));
         for J in I + 1 .. Num_Bars loop
            Steps.Append (Sort_Step_T'(Compare, J, Min, 0, 7));
            if A (J) < A (Min) then
               Min := J;
            end if;
         end loop;
         if Min /= I then
            Steps.Append (Sort_Step_T'(Swap, I, Min, 0, 11));
            Tmp := A (I); A (I) := A (Min); A (Min) := Tmp;
         end if;
         Steps.Append (Sort_Step_T'(Mark_Sorted, I, 0, 0, 14));
      end loop;
      Steps.Append (Sort_Step_T'(Mark_Sorted, Num_Bars, 0, 0, 15));
      Steps.Append (Sort_Step_T'(Mark_All_Sorted, 0, 0, 0, 15));
   end Record_Selection;

   procedure Record_Quick is
      A : Bar_Array_T := Bars;

      procedure QS (Lo, Hi : Natural) is
         Pivot : Positive;
         P     : Natural;
         Tmp   : Positive;
      begin
         if Lo < Hi then
            Pivot := A (Hi);
            P := Lo - 1;
            Steps.Append (Sort_Step_T'(Compare, Hi, 0, 0, 5));
            for J in Lo .. Hi - 1 loop
               Steps.Append (Sort_Step_T'(Compare, J, Hi, 0, 8));
               if A (J) <= Pivot then
                  P := P + 1;
                  if P /= J then
                     Steps.Append (Sort_Step_T'(Swap, P, J, 0, 10));
                     Tmp := A (P); A (P) := A (J); A (J) := Tmp;
                  end if;
               end if;
            end loop;
            Steps.Append (Sort_Step_T'(Swap, P + 1, Hi, 0, 13));
            Tmp := A (P + 1); A (P + 1) := A (Hi); A (Hi) := Tmp;
            Steps.Append (Sort_Step_T'(Mark_Sorted, P + 1, 0, 0, 13));
            QS (Lo, P);
            QS (P + 2, Hi);
         elsif Lo = Hi then
            Steps.Append (Sort_Step_T'(Mark_Sorted, Lo, 0, 0, 4));
         end if;
      end QS;

   begin
      Steps.Clear;
      QS (1, Num_Bars);
      Steps.Append (Sort_Step_T'(Mark_All_Sorted, 0, 0, 0, 17));
   end Record_Quick;

   procedure Record_Merge is
      A : Bar_Array_T := Bars;

      procedure MS (Lo, Hi : Natural) is
         Mid : Natural;
         L   : Bar_Array_T;
         R   : Bar_Array_T;
         Li, Ri, K : Natural;
         L_Len, R_Len : Natural;
      begin
         if Lo < Hi then
            Mid := (Lo + Hi) / 2;
            Steps.Append (Sort_Step_T'(Compare, Lo, Hi, 0, 5));
            MS (Lo, Mid);
            MS (Mid + 1, Hi);
            L_Len := Mid - Lo + 1;
            R_Len := Hi - Mid;
            for I in 1 .. L_Len loop
               L (I) := A (Lo + I - 1);
            end loop;
            for I in 1 .. R_Len loop
               R (I) := A (Mid + I);
            end loop;
            Li := 1; Ri := 1; K := Lo;
            while Li <= L_Len and then Ri <= R_Len loop
               Steps.Append (Sort_Step_T'(Compare, K, 0, 0, 12));
               if L (Li) <= R (Ri) then
                  Steps.Append (Sort_Step_T'(Set_Value, K, 0, L (Li), 13));
                  A (K) := L (Li); Li := Li + 1;
               else
                  Steps.Append (Sort_Step_T'(Set_Value, K, 0, R (Ri), 15));
                  A (K) := R (Ri); Ri := Ri + 1;
               end if;
               K := K + 1;
            end loop;
            while Li <= L_Len loop
               Steps.Append (Sort_Step_T'(Set_Value, K, 0, L (Li), 18));
               A (K) := L (Li); Li := Li + 1; K := K + 1;
            end loop;
            while Ri <= R_Len loop
               Steps.Append (Sort_Step_T'(Set_Value, K, 0, R (Ri), 18));
               A (K) := R (Ri); Ri := Ri + 1; K := K + 1;
            end loop;
         end if;
      end MS;

   begin
      Steps.Clear;
      MS (1, Num_Bars);
      Steps.Append (Sort_Step_T'(Mark_All_Sorted, 0, 0, 0, 20));
   end Record_Merge;

   procedure Record_Stalin is
      A   : constant Bar_Array_T := Bars;
      Max : Positive;
   begin
      Steps.Clear;
      Max := A (1);
      Steps.Append (Sort_Step_T'(Mark_Sorted, 1, 0, 0, 4));
      for I in 2 .. Num_Bars loop
         Steps.Append (Sort_Step_T'(Compare, I, I - 1, 0, 6));
         if A (I) >= Max then
            Max := A (I);
            Steps.Append (Sort_Step_T'(Mark_Sorted, I, 0, 0, 7));
         else
            Steps.Append (Sort_Step_T'(Mark_Deleted, I, 0, 0, 10));
         end if;
      end loop;
   end Record_Stalin;

   procedure Record_Steps is
   begin
      Steps.Clear;
      case Current_Algo is
         when Bubble_Sort    => Record_Bubble;
         when Insertion_Sort => Record_Insertion;
         when Selection_Sort => Record_Selection;
         when Quick_Sort     => Record_Quick;
         when Merge_Sort     => Record_Merge;
         when Stalin_Sort    => Record_Stalin;
      end case;
   end Record_Steps;

   ---------------------------------------------------------------
   --  APPLY ONE STEP
   ---------------------------------------------------------------

   procedure Apply_Step is
      S   : Sort_Step_T;
      Tmp : Positive;
   begin
      if Current_Step >= Natural (Steps.Length) then
         Is_Finished := True;
         for I in 1 .. Num_Bars loop
            if Bar_States (I) /= Deleted then
               Bar_States (I) := Sorted;
            end if;
         end loop;
         Active_Line := 0;
         return;
      end if;

      S := Steps (Current_Step);
      Active_Line := S.Code_Line;

      for I in 1 .. Num_Bars loop
         if Bar_States (I) /= Sorted
            and then Bar_States (I) /= Deleted
         then
            Bar_States (I) := Unsorted;
         end if;
      end loop;

      case S.Action is
         when Compare =>
            if S.Index_A in 1 .. Num_Bars
               and then Bar_States (S.Index_A) /= Deleted
            then
               Bar_States (S.Index_A) := Comparing;
            end if;
            if S.Index_B in 1 .. Num_Bars
               and then Bar_States (S.Index_B) /= Deleted
            then
               Bar_States (S.Index_B) := Comparing;
            end if;

         when Swap =>
            if S.Index_A in 1 .. Num_Bars
               and then S.Index_B in 1 .. Num_Bars
            then
               Tmp := Bars (S.Index_A);
               Bars (S.Index_A) := Bars (S.Index_B);
               Bars (S.Index_B) := Tmp;
               Bar_States (S.Index_A) := Swapping;
               Bar_States (S.Index_B) := Swapping;
            end if;

         when Set_Value =>
            if S.Index_A in 1 .. Num_Bars
               and then S.Value in 1 .. Num_Bars
            then
               Bars (S.Index_A) := S.Value;
               Bar_States (S.Index_A) := Swapping;
            end if;
            if S.Index_B in 1 .. Num_Bars then
               Bar_States (S.Index_B) := Comparing;
            end if;

         when Mark_Sorted =>
            if S.Index_A in 1 .. Num_Bars then
               Bar_States (S.Index_A) := Sorted;
            end if;

         when Mark_All_Sorted =>
            for I in 1 .. Num_Bars loop
               if Bar_States (I) /= Deleted then
                  Bar_States (I) := Sorted;
               end if;
            end loop;
            Is_Finished := True;

         when Mark_Deleted =>
            if S.Index_A in 1 .. Num_Bars then
               Bar_States (S.Index_A) := Deleted;
            end if;
      end case;

      Current_Step := Current_Step + 1;
   end Apply_Step;

   ---------------------------------------------------------------
   --  LEGEND
   ---------------------------------------------------------------

   type Legend_Entry_T is record
      Color : Color_t;
      Label : Unbounded_String;
   end record;

   Max_Legend_Entries : constant := 5;
   type Legend_Array_T is array (1 .. Max_Legend_Entries) of Legend_Entry_T;

   type Legend_T is record
      Entries : Legend_Array_T;
      Count   : Natural := 0;
   end record;

   function Get_Legend return Legend_T is
      L : Legend_T;

      procedure Add (C : Color_t; S : String) is
      begin
         L.Count := L.Count + 1;
         L.Entries (L.Count) := (Color => C,
                                 Label => To_Unbounded_String (S));
      end Add;
   begin
      case Current_Algo is
         when Bubble_Sort =>
            Add (Color_Bar_Normal, "Unsorted");
            Add (Color_Bar_Cmp,    "Comparing");
            Add (Color_Bar_Swap,   "Swapping");
            Add (Color_Bar_Done,   "Sorted (final position)");
         when Insertion_Sort =>
            Add (Color_Bar_Normal, "Unsorted");
            Add (Color_Bar_Cmp,    "Comparing with key");
            Add (Color_Bar_Swap,   "Shifting / inserting");
            Add (Color_Bar_Done,   "Sorted");
         when Selection_Sort =>
            Add (Color_Bar_Normal, "Unsorted");
            Add (Color_Bar_Cmp,    "Scanning for minimum");
            Add (Color_Bar_Swap,   "Swapping with minimum");
            Add (Color_Bar_Done,   "Sorted (final position)");
         when Quick_Sort =>
            Add (Color_Bar_Normal, "Unsorted");
            Add (Color_Bar_Cmp,    "Comparing with pivot");
            Add (Color_Bar_Swap,   "Swapping (partition)");
            Add (Color_Bar_Done,   "Sorted (pivot placed)");
         when Merge_Sort =>
            Add (Color_Bar_Normal, "Unsorted");
            Add (Color_Bar_Cmp,    "Comparing during merge");
            Add (Color_Bar_Swap,   "Writing merged value");
            Add (Color_Bar_Done,   "Sorted");
         when Stalin_Sort =>
            Add (Color_Bar_Normal, "Unsorted");
            Add (Color_Bar_Cmp,    "Comparing with max");
            Add (Color_Bar_Done,   "Survived");
            Add (Color_Bar_BG,     "Deleted (to the gulag!)");
      end case;
      return L;
   end Get_Legend;

   ---------------------------------------------------------------
   --  PUBLIC INTERFACE
   ---------------------------------------------------------------

   procedure Initialise is
   begin
      Init_Source;
      Shuffle_Array;
      Record_Steps;
   end Initialise;

   procedure Reset is
   begin
      Shuffle_Array;
      Record_Steps;
      Current_Step   := 0;
      Active_Line    := 0;
      Is_Paused      := True;
      Is_Finished    := False;
      Last_Step_Time := Ada.Real_Time.Clock;
   end Reset;

   procedure Switch_Algo (N : Positive) is
   begin
      case N is
         when 1 => Current_Algo := Bubble_Sort;
         when 2 => Current_Algo := Insertion_Sort;
         when 3 => Current_Algo := Selection_Sort;
         when 4 => Current_Algo := Quick_Sort;
         when 5 => Current_Algo := Merge_Sort;
         when 6 => Current_Algo := Stalin_Sort;
         when others => return;
      end case;
      Reset;
   end Switch_Algo;

   procedure Toggle_Play is
   begin
      if Is_Finished then
         Reset;
      else
         Is_Paused := not Is_Paused;
         Last_Step_Time := Ada.Real_Time.Clock;
      end if;
   end Toggle_Play;

   procedure Step_Forward is
   begin
      if Is_Paused and then not Is_Finished then
         Apply_Step;
      end if;
   end Step_Forward;

   procedure Speed_Down is
   begin
      if Delay_MS < 1000 then
         Delay_MS := Delay_MS + 10;
      end if;
   end Speed_Down;

   procedure Speed_Up is
   begin
      if Delay_MS > 10 then
         Delay_MS := Delay_MS - 10;
      end if;
   end Speed_Up;

   procedure Tick is
      Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      if not Is_Paused and then not Is_Finished then
         if Now - Last_Step_Time >=
            Ada.Real_Time.Milliseconds (Delay_MS)
         then
            Apply_Step;
            Last_Step_Time := Now;
         end if;
      end if;
   end Tick;

   function Algo_Display_Name return String is
   begin
      return Algo_Name (Current_Algo);
   end Algo_Display_Name;

   function Is_Running return Boolean is
   begin
      return not Is_Paused and then not Is_Finished;
   end Is_Running;

   function Help_Text return String is
   begin
      return
        " [1-6] Algo  [SPC] Play/Pause  [N] Step"
        & "  [+/-] Speed  [R] Reset  |  [/]: tab";
   end Help_Text;

   ---------------------------------------------------------------
   --  RENDER: BAR CHART
   ---------------------------------------------------------------

   procedure Render_Bars (Buf : in out Buffer_T;
                          W   : in     TUI_Width;
                          H   : in     TUI_Height) is
      Bx      : TUI_Width;
      Bh      : Natural;
      Clr     : Color_t;
      Max_H   : constant Natural := Natural (H) - 2;
   begin
      for X in TUI_Width'First .. W loop
         for Y in TUI_Height'First .. H loop
            Set_Buffer_Pixel (Buf, X, Y,
               (Char => ' ', Background_Color => Color_Bar_BG, others => <>));
         end loop;
      end loop;

      for I in 1 .. Num_Bars loop
         Bx := TUI_Width (I);
         exit when Natural (Bx) > Natural (W);

         Bh := (Bars (I) * Max_H) / Num_Bars;
         if Bh < 1 then Bh := 1; end if;
         Clr := State_Color (Bar_States (I));

         for Y in TUI_Height'First .. H loop
            if Natural (Y) > Natural (H) - Bh then
               Set_Buffer_Pixel (Buf, Bx, Y,
                  (Char => ' ', Background_Color => Clr, others => <>));
            end if;
         end loop;
      end loop;
   end Render_Bars;

   ---------------------------------------------------------------
   --  RENDER: CODE PANEL
   ---------------------------------------------------------------

   procedure Render_Code (Buf : in out Buffer_T;
                          W   : in     TUI_Width;
                          H   : in     TUI_Height) is
      Src  : constant Source_Code_T := Get_Source;
      Leg  : constant Legend_T := Get_Legend;
      BG   : Color_t;
      FG   : Color_t;
      Line : Unbounded_String;
      Y    : TUI_Height;
      Legend_Start_Row : Natural;
   begin
      for X in TUI_Width'First .. W loop
         for Yr in TUI_Height'First .. H loop
            Set_Buffer_Pixel (Buf, X, Yr,
               (Char => ' ', Background_Color => Color_Code_BG, others => <>));
         end loop;
      end loop;

      --  Header
      declare
         Hdr : constant String := " " & Algo_Name (Current_Algo);
      begin
         for C in 1 .. Hdr'Length loop
            exit when C > Natural (W);
            Set_Buffer_Pixel (Buf, TUI_Width (C), 1,
               (Char       => Hdr (C),
                Char_Color => Color_Code_Hdr,
                Background_Color => Color_Code_BG,
                Is_Bold    => True,
                others     => False));
         end loop;
      end;

      --  Source lines starting at row 3
      for L in 1 .. Src.Count loop
         Y := TUI_Height (L + 2);
         exit when Natural (Y) > Natural (H);

         if L = Active_Line then
            BG := Color_Code_HL;
         else
            BG := Color_Code_BG;
         end if;
         FG := Color_Code_Txt;

         for X in TUI_Width'First .. W loop
            Set_Buffer_Pixel (Buf, X, Y,
               (Char => ' ', Char_Color => FG,
                Background_Color => BG, others => <>));
         end loop;

         --  Line number
         declare
            Num : constant String := Img (L);
            Col : Natural;
         begin
            if L < 10 then Col := 2; else Col := 1; end if;
            for C in Num'Range loop
               exit when Col > 3;
               Set_Buffer_Pixel (Buf, TUI_Width (Col), Y,
                  (Char       => Num (C),
                   Char_Color => Gray,
                   Background_Color => BG,
                   others     => <>));
               Col := Col + 1;
            end loop;
         end;

         --  Source text starting at column 5
         Line := Src.Lines (L);
         for C in 1 .. Length (Line) loop
            exit when C + 4 > Natural (W);
            Set_Buffer_Pixel (Buf, TUI_Width (C + 4), Y,
               (Char       => Element (Line, C),
                Char_Color => FG,
                Background_Color => BG,
                others     => <>));
         end loop;
      end loop;

      --  Legend
      Legend_Start_Row := Src.Count + 2 + 2;

      if Legend_Start_Row <= Natural (H) then
         declare
            Hdr : constant String := " Color Key:";
         begin
            for C in 1 .. Hdr'Length loop
               exit when C > Natural (W);
               Set_Buffer_Pixel (Buf, TUI_Width (C),
                  TUI_Height (Legend_Start_Row),
                  (Char       => Hdr (C),
                   Char_Color => Color_Code_Hdr,
                   Background_Color => Color_Code_BG,
                   Is_Bold    => True,
                   others     => False));
            end loop;
         end;
      end if;

      for E in 1 .. Leg.Count loop
         declare
            Row : constant Natural := Legend_Start_Row + E;
            Lbl : constant String  := To_String (Leg.Entries (E).Label);
            Swatch_Color : constant Color_t := Leg.Entries (E).Color;
         begin
            exit when Row > Natural (H);

            for SX in 2 .. 3 loop
               Set_Buffer_Pixel (Buf, TUI_Width (SX), TUI_Height (Row),
                  (Char             => ' ',
                   Background_Color => Swatch_Color,
                   others           => <>));
            end loop;

            for C in 1 .. Lbl'Length loop
               exit when C + 4 > Natural (W);
               Set_Buffer_Pixel (Buf, TUI_Width (C + 4), TUI_Height (Row),
                  (Char       => Lbl (Lbl'First + C - 1),
                   Char_Color => Color_Code_Txt,
                   Background_Color => Color_Code_BG,
                   others     => <>));
            end loop;
         end;
      end loop;
   end Render_Code;

   ---------------------------------------------------------------
   --  RENDER: STATUS BAR
   ---------------------------------------------------------------

   procedure Render_Status (Buf : in out Buffer_T;
                            W   : in     TUI_Width;
                            H   : in     TUI_Height) is
      State_Str : constant String :=
         (if Is_Finished then "DONE   "
          elsif Is_Paused then "PAUSED "
          else "RUNNING");
      L1 : constant String :=
         " " & Algo_Name (Current_Algo) & " | " & State_Str
         & " | Step " & Img (Current_Step) & "/"
         & Img (Natural (Steps.Length))
         & " | Delay: " & Img (Delay_MS) & "ms"
         & " | Bars: " & Img (Num_Bars);
      L2 : constant String :=
         " [1-6] Algo  [SPC] Play/Pause  [N] Step"
         & "  [+/-] Speed  [R] Reset  [ESC] Quit";

      procedure Write_Line (Row : TUI_Height; Text : String) is
      begin
         for C in 1 .. Natural (W) loop
            if C <= Text'Length then
               Set_Buffer_Pixel (Buf, TUI_Width (C), Row,
                  (Char       => Text (Text'First + C - 1),
                   Char_Color => White,
                   Background_Color => Color_Status_BG,
                   others     => <>));
            else
               Set_Buffer_Pixel (Buf, TUI_Width (C), Row,
                  (Char       => ' ',
                   Background_Color => Color_Status_BG,
                   others     => <>));
            end if;
         end loop;
      end Write_Line;

   begin
      Write_Line (1, L1);
      if Natural (H) >= 2 then
         Write_Line (2, L2);
      end if;
   end Render_Status;

end Sort_Demo;
