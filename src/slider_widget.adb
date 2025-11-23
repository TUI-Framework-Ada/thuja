-- Using: https://learn.adacore.com/courses/intro-to-ada/chapters/standard_library_containers.html

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers; use Ada.Containers;
with Entities; use Entities;

package body Slider_Widget is

   ------------------------------------------------------------
   -- Custom Hash Function for Entity_Id
   ------------------------------------------------------------
   -- Required by Indefinite_Hashed_Maps to convert Entity_Id
   -- into a hash value. This is portable and standards-compliant.
   function Entity_Hash (Key : Entity_Id) return Hash_Type is
   begin
      -- Simple and valid hash since Entity_Id is numeric
      return Hash_Type (Key);
   end Entity_Hash;


   ------------------------------------------------------------
   -- Map: Entity_Id -> Slider Value (0..100)
   ------------------------------------------------------------
   -- Stores slider value for each widget entity
   package Value_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => Entity_Id,     -- Entity Identifier from Entities package
      Element_Type    => Integer,       -- Slider value
      Hash            => Entity_Hash,   -- Our custom hash function
      Equivalent_Keys => "=");

   -- Actual container instance holding all slider values
   Values : Value_Map.Map;


   ------------------------------------------------------------
   -- Create Slider Widget
   ------------------------------------------------------------
   function Create return Entity_Id is
      Id : Entities.Entity_Id;
   begin
      Id := Entities.Create;
      Values.Insert (Id, 50); -- Start in the middle
      return Id;
   end Create;


   ------------------------------------------------------------
   -- Draw Slider Widget
   ------------------------------------------------------------
   procedure Draw (Id : Entity_Id) is
      Value  : Integer;
      Filled : Integer;
   begin
      -- Don’t draw if entity is dead (saves cycles + prevents errors)
      if not Is_Alive (Id) then
         return;
      end if;

      Value  := Values.Element (Id);  -- Get current slider value
      Filled := Value / 10;           -- Each '=' represents 10 units

      -- Interactive slider representation
      Put ("[");
      for I in 1 .. 10 loop
         if I <= Filled then
            Put ("=");
         else
            Put (" ");
         end if;
      end loop;

      Put_Line ("] " & Integer'Image(Value));
   end Draw;


   ------------------------------------------------------------
   -- Handle_Input
   ------------------------------------------------------------
   procedure Handle_Input (Id : Entity_Id; Key : Character) is
      Value : Integer;
   begin
      if not Is_Alive (Id) then
         return;
      end if;

      Value := Values.Element (Id);

      -- Simple input: 'a' to decrease, 'd' to increase
      -- Can be changed later for arrow keys or advanced control
      case Key is
         when 'a' =>
            if Value > 0 then
               Value := Value - 5;
            end if;

         when 'd' =>
            if Value < 100 then
               Value := Value + 5;
            end if;

         when others =>
            null;
      end case;

      -- Save updated value into the map
      Values.Replace (Id, Value);
   end Handle_Input;

end Slider_Widget;
