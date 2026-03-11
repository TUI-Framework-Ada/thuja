with Input_Handling; use Input_Handling;
with Ada.Text_IO;

procedure Input_Demo is
   type Demo_State_t is (Welcome, Running, Exiting, Done);
   Current_State : Demo_State_t := Welcome;
   Cmd : Command_t;
begin
   --  Start the input reader task
   Input_Reader.Start;

   loop
      case Current_State is
         when Welcome =>
            Ada.Text_IO.Put_Line ("=== Input Handling Demo ===");
            Ada.Text_IO.Put_Line ("Press Tab to switch modes");
            Ada.Text_IO.Put_Line ("Press Enter to confirm");
            Ada.Text_IO.Put_Line ("Press Q to quit");
            Ada.Text_IO.New_Line;
            Current_State := Running;

         when Running =>
            --  Get command from protected buffer
            Input_Buffer.Consume (Cmd);

            case Cmd is
               when Tab =>
                  Ada.Text_IO.Put_Line ("Tab pressed!");

               when Enter =>
                  Ada.Text_IO.Put_Line ("Enter pressed!");

               when Quit =>
                  Ada.Text_IO.Put_Line ("Quitting...");
                  Current_State := Exiting;

               when None =>
                  null;  --  No input available
            end case;

            delay 0.05;  --  Small delay to prevent busy loop

         when Exiting =>
            Input_Reader.Stop;
            Current_State := Done;

         when Done =>
            Ada.Text_IO.Put_Line ("Demo complete!");
            exit;
      end case;
   end loop;
end Input_Demo;
