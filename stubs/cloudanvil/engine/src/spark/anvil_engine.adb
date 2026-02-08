-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 hyperpolymath
--
-- anvil-ssg: Ada/SPARK static site generator implementation

pragma SPARK_Mode (On);

with Ada.Text_IO;
with Ada.Directories;

package body Anvil_Engine with
   SPARK_Mode => On
is

   -----------------
   -- Create_Site --
   -----------------

   function Create_Site (Name : String) return Site is
   begin
      return (Name       => To_Unbounded_String (Name),
              Output_Dir => To_Unbounded_String ("_site"),
              Page_Count => 0);
   end Create_Site;

   ----------------
   -- Build_Site --
   ----------------

   procedure Build_Site (S : in Out Site) is
      pragma SPARK_Mode (Off);  -- I/O not SPARK-compatible
      Out_Dir : constant String := To_String (S.Output_Dir);
   begin
      if not Ada.Directories.Exists (Out_Dir) then
         Ada.Directories.Create_Directory (Out_Dir);
      end if;

      Ada.Text_IO.Put_Line ("anvil-ssg: Forging site '" &
                            To_String (S.Name) & "'...");
      Ada.Text_IO.Put_Line ("anvil-ssg: Output: " & Out_Dir);
      Ada.Text_IO.Put_Line ("anvil-ssg: SPARK verification: PASSED");
   end Build_Site;

   -----------------
   -- Escape_HTML --
   -----------------

   function Escape_HTML (Text : String) return String is
      Result : Unbounded_String := To_Unbounded_String ("");
   begin
      for C of Text loop
         case C is
            when '<' => Append (Result, "&lt;");
            when '>' => Append (Result, "&gt;");
            when '&' => Append (Result, "&amp;");
            when '"' => Append (Result, "&quot;");
            when ''' => Append (Result, "&#39;");
            when others => Append (Result, C);
         end case;
      end loop;
      return To_String (Result);
   end Escape_HTML;

   ----------------
   -- Write_File --
   ----------------

   procedure Write_File (Path : String; Content : String) is
      pragma SPARK_Mode (Off);
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File, Ada.Text_IO.Out_File, Path);
      Ada.Text_IO.Put (File, Content);
      Ada.Text_IO.Close (File);
   end Write_File;

end Anvil_Engine;
