with Command_Sequence_Handling; use Command_Sequence_Handling;
with Ada.Strings.Unbounded;

procedure Demos_Tests.Command_Sequence is
   package SU renames Ada.Strings.Unbounded;

   R : Handler_Result_t;
begin
   --  ========================================
   --  Test 1: Single key that is a prefix
   --  should return No_Result (keep buffering)
   --  ========================================
   Initialize;
   R := Process_Key ('a');
   pragma Assert (R.Kind = No_Result,
      "Expected No_Result for 'a' (prefix of A -> B)");

   --  ========================================
   --  Test 2: Completing A -> B fires command
   --  ========================================
   R := Process_Key ('b');
   pragma Assert (R.Kind = Command_Activated,
      "Expected Command_Activated for 'a','b'");
   pragma Assert (R.Command_Index = 1,
      "Expected Command_Index = 1 for A -> B");
   pragma Assert (SU.To_String (R.Keys) = "ab",
      "Expected keys 'ab' for A -> B");

   --  ========================================
   --  Test 3: C -> D sequence
   --  ========================================
   Initialize;
   R := Process_Key ('c');
   pragma Assert (R.Kind = No_Result,
      "Expected No_Result for 'c' (prefix of C -> D)");
   R := Process_Key ('d');
   pragma Assert (R.Kind = Command_Activated,
      "Expected Command_Activated for 'c','d'");
   pragma Assert (R.Command_Index = 2,
      "Expected Command_Index = 2 for C -> D");

   --  ========================================
   --  Test 4: Three-key sequence E -> F -> G
   --  ========================================
   Initialize;
   R := Process_Key ('e');
   pragma Assert (R.Kind = No_Result,
      "Expected No_Result for 'e' (prefix of E -> F -> G)");
   R := Process_Key ('f');
   pragma Assert (R.Kind = No_Result,
      "Expected No_Result for 'e','f' (still a prefix)");
   R := Process_Key ('g');
   pragma Assert (R.Kind = Command_Activated,
      "Expected Command_Activated for 'e','f','g'");
   pragma Assert (R.Command_Index = 3,
      "Expected Command_Index = 3 for E -> F -> G");
   pragma Assert (SU.To_String (R.Keys) = "efg",
      "Expected keys 'efg' for E -> F -> G");

   --  ========================================
   --  Test 5: Non-matching key passes through
   --  immediately (no prefix match at all)
   --  ========================================
   Initialize;
   R := Process_Key ('x');
   pragma Assert (R.Kind = Keys_Passed_Through,
      "Expected Keys_Passed_Through for 'x'");
   pragma Assert (SU.To_String (R.Keys) = "x",
      "Expected passed-through key 'x'");

   --  ========================================
   --  Test 6: Partial prefix then non-matching
   --  key passes through entire buffer
   --  ========================================
   Initialize;
   R := Process_Key ('a');
   pragma Assert (R.Kind = No_Result,
      "Expected No_Result for 'a'");
   R := Process_Key ('x');
   pragma Assert (R.Kind = Keys_Passed_Through,
      "Expected Keys_Passed_Through for 'a','x'");
   pragma Assert (SU.To_String (R.Keys) = "ax",
      "Expected passed-through keys 'ax'");

   --  ========================================
   --  Test 7: Case insensitivity
   --  Uppercase input should match lowercase
   --  command table entries
   --  ========================================
   Initialize;
   R := Process_Key ('A');
   pragma Assert (R.Kind = No_Result,
      "Expected No_Result for 'A' (case insensitive prefix)");
   R := Process_Key ('B');
   pragma Assert (R.Kind = Command_Activated,
      "Expected Command_Activated for 'A','B' (case insensitive)");

   --  ========================================
   --  Test 8: Sequential commands
   --  State resets after a command fires, so
   --  a new sequence can start immediately
   --  ========================================
   Initialize;
   R := Process_Key ('c');
   pragma Assert (R.Kind = No_Result, "Expected No_Result for 'c'");
   R := Process_Key ('d');
   pragma Assert (R.Kind = Command_Activated,
      "Expected Command_Activated for C -> D");
   --  New sequence starts fresh after firing
   R := Process_Key ('e');
   pragma Assert (R.Kind = No_Result,
      "Expected No_Result for 'e' after previous command fired");
   R := Process_Key ('f');
   pragma Assert (R.Kind = No_Result,
      "Expected No_Result for 'e','f'");
   R := Process_Key ('g');
   pragma Assert (R.Kind = Command_Activated,
      "Expected Command_Activated for E -> F -> G after C -> D");

   --  ========================================
   --  Test 9: Get_Command_Name correctness
   --  ========================================
   pragma Assert (Get_Command_Name (1) = "A -> B",
      "Expected name 'A -> B' for index 1");
   pragma Assert (Get_Command_Name (2) = "C -> D",
      "Expected name 'C -> D' for index 2");
   pragma Assert (Get_Command_Name (3) = "E -> F -> G",
      "Expected name 'E -> F -> G' for index 3");
   pragma Assert (Get_Command_Name (0) = "Unknown",
      "Expected 'Unknown' for invalid index 0");

   --  ========================================
   --  Test 10: Pass-through after deep prefix
   --  Buffer 'e','f' is a prefix but 'e','f','x'
   --  is not — all three keys pass through
   --  ========================================
   Initialize;
   R := Process_Key ('e');
   pragma Assert (R.Kind = No_Result, "Expected No_Result for 'e'");
   R := Process_Key ('f');
   pragma Assert (R.Kind = No_Result, "Expected No_Result for 'e','f'");
   R := Process_Key ('x');
   pragma Assert (R.Kind = Keys_Passed_Through,
      "Expected Keys_Passed_Through for 'e','f','x'");
   pragma Assert (SU.To_String (R.Keys) = "efx",
      "Expected passed-through keys 'efx'");

end Demos_Tests.Command_Sequence;
